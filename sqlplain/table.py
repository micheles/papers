import sys, string
from sqlplain import util, do
from sqlplain.namedtuple import namedtuple

# kfields and dfields must be tuples, not strings
def tabletuple(name, kfields, dfields):
    """
    Returns a namedtuple with attributes ._kfields, ._dfields and properties
    ._kvalues, .dvalues. 
    """
    ttuple = namedtuple(name, kfields + dfields)
    ttuple._ktuple = ktuple = namedtuple(name + '_key', kfields)
    ttuple._dtuple = dtuple = namedtuple(name + '_data', dfields)
    ttuple._kfields = ktuple._fields
    ttuple._dfields = dtuple._fields
    ttuple._kvalues = property(
        lambda self: ktuple(*[getattr(self, n) for n in self._kfields]))
    ttuple._dvalues = property(
        lambda self: dtuple(*[getattr(self, n) for n in self._dfields]))
    return ttuple

# closures to be instantiated in DTable.__init__

def insert(ttuple):
    "Return a procedure inserting a row or a dictionary into a table"
    name = ttuple.__name__
    fields = ttuple._fields
    csfields = ', '.join(fields)
    qmarks = ', '.join('?'*len(fields))
    templ = 'INSERT INTO %s (%s) VALUES (%s)' % (name, csfields, qmarks)
    def insert_row(conn, row=None, **kw):
        row = row or {}
        if isinstance(row, dict):
            row.update(kw)
            missing = set(fields) - set(row) # check for a better error message
            if missing:
                raise TypeError('Missing field(s) %s' % ', '.join(missing)) 
            row = ttuple(**row)
        return conn.execute(templ, row)
    insert_row.__doc__ = insert_row.templ = templ
    return insert_row

def select(ttuple):
    """
    Returns a function with signature (conn, key) - where key can be
    a dictionary or a tabletuple - returning a single row.
    """
    name = ttuple.__name__
    csfields = ','.join(ttuple._fields)
    clause = ' AND '.join('%s=?' % field for field in ttuple._kfields)
    templ = 'SELECT %s FROM %s WHERE %s' % (csfields, name, clause)
    def select_row(conn, row=None, **kw):
        row = row or {}
        if isinstance(row, dict):
            row.update(kw)
            row = ttuple._ktuple(**row)        
        res = conn.execute(templ, row, ttuple)
        if not res:
            raise KeyError('Missing record for %s' % str(row))
        elif len(res) > 1:
            raise RuntimeError('Got %s instead of a single row' % res)
        return res[0]
    select_row.__doc__ = select_row.templ = templ
    return select_row

def delete(ttuple):
    "Return a procedure inserting a row or a dictionary into a table"
    name = ttuple.__name__
    clause = ' AND '.join('%s=?' % field for field in ttuple._kfields)
    templ = 'DELETE FROM %s WHERE %s' % (name, clause)
    def delete_row(conn, row=None, **kw):
        row = row or {}
        if isinstance(row, dict):
            row.update(kw)
            row = ttuple._ktuple(**row)        
        return conn.execute(templ, row, ttuple)
    delete_row.__doc__ = delete_row.templ = templ
    return delete_row

def update(ttuple):
    "Returns a procedure updating a row"
    name = ttuple.__name__
    where = ' AND '.join('%s=?' % field for field in ttuple._kfields)
    templ = string.Template('UPDATE %s SET $set WHERE %s' % (name, where))
    def update_row(conn, row=None, **kw):
        if row is None:
            row = {}
        elif hasattr(row, '_asdict'):
            row = row._asdict()
        row.update(kw)
        kvalues, dvalues, dfields = [], [], []
        for f, v in row.iteritems():
            if f in ttuple._kfields:
                kvalues.append(v)
            else:
                dvalues.append(v)
                dfields.append(f)
        sql = templ.substitute(set=', '.join('%s=?' % f for f in dfields))
        return conn.execute(sql, dvalues + kvalues)
    update_row.__doc__ = update_row.templ = templ
    return update_row

def update_or_insert(ttuple):
    "Returns a procedure updating or inserting a row"
    up = update(ttuple)
    ins = insert(ttuple)
    def update_or_insert_row(conn, row=None, **kw):
        n = up(conn, row, **kw)
        if n == 0:
            n = ins(conn, row, **kw)
        return n
    update_or_insert_row.__doc__ = update_or_insert_row.templ = None
    return update_or_insert_row
    
class DView(object):
    """
    """

    @classmethod
    def create(cls, conn, name, fields, force=False):
        util.create_view(conn, name, fields, force)
        return cls(conn, name)

    def __init__(self, conn, name, fields=(), query=''):
        self.conn = conn
        self.name = name
        if query:
            self.query = '(%s) AS %s' % (query, name)
            s = 'SELECT * FROM %s WHERE 1=0' % self.query
            fields = [r.name for name in conn.execute(s).descr]
        else:
            self.query = name
        fields = fields or util.get_fields(conn, name)
        self.tt = tabletuple(name, fields)

    def select(self, clause='', *args):
        "Select rows from the table"
        fields = ', '.join(self.tt._fields)
        templ = 'SELECT %s FROM %s %s' % (fields, self.query, clause)
        if args:
            return do(templ, ntuple=self.tt)(self.conn, templ, *args)
        else:
            return self.conn.execute(templ, ntuple=self.tt)

    def count(self, clause=''):
        "Count the number of rows satisfying the given clause"
        templ = 'SELECT COUNT(*) FROM %s %s' % (self.query, clause)
        if args:
            return do(templ)(self.conn, templ, *args)
        return self.conn.execute(templ, scalar=True)

    def __iter__(self):
        return iter(self.select())

    def __len__(self):
        "Return the total number of rows in the table"
        return self.count()
    
class DTable(DView):
    """
    A simple table class for database tables without a primary key.
    The only methods are insert_row, insert_file, delete, truncate, select.
    """

    @classmethod
    def create(cls, conn, name, body, force=False):
        util.create_table(conn, name, body, force)
        return cls(conn, name)

    def insert_rows(self, rows):
        'Populate a table by reading a row-iterator'
        return util.insert_rows(self.conn, self.name, rows)

    def insert_file(self, file, sep='1t'):
        'Populate a table by reading a file-like object'
        return util.insert_file(self.conn, file, self.name, sep)

    def __init__(self, conn, name, fields=()):
        self.conn = conn
        self.name = self.query = name
        if not fields:
            fields = util.get_fields(conn, name)
        self.tt = namedtuple(name, fields)
        self.insert_row = insert(self.tt)

    def delete(self, clause=''):
        "Delete rows from the table"
        templ = 'DELETE FROM %s %s' % (self.name, clause)
        if args:
            return do(templ)(self.conn, templ, *args)
        return self.conn.execute(templ)

    def truncate(self):
        "Truncate the table"
        return util.truncate_table(self.conn, self.name)
    
class KTable(DTable):
    """
    An object oriented wrapper for database tables with a primary key.
    """
    
    def __init__(cls, name, kfields=(), dfields=()):
        "Ex. Book = KTable.type('book', 'serial', 'title author')"
        kfields = kfields or util.get_kfields(conn, name)
        dfields = dfields or util.get_dfields(conn, name)
        if not kfields:
            raise TypeError('table %s has no primary key!' % name)
        self.tt = tabletuple(name, kfields, dfields)
        for nam in ('insert', 'delete', 'select', 'update', 'update_or_insert'):
            func = globals()[nam](self.tt)
            setattr(self, nam + '_row', func)

    def __contains__(self, key):
        try:
            self.select_row(key)
        except KeyError:
            return False
        else:
            return True
    
    def keyset(self):
        """Return a set with the key(s) of the table"""
        kfields = ', '.join(self.tt._kfields)
        return set(self.conn.execute(
            'SELECT %s FROM %s' % (kfields, self.name)))

if __name__ == '__main__':
    tt = tabletuple('tt', 'x y', 'a,b')(1, 2, 3, 4)
    print tt._kvalues, tt._dvalues
    print tt.__class__.mro()

    Book = KTable.type('book', 'pubdate', 'title author')
    from sqlplain import lazyconnect
    conn = lazyconnect('srs_dev')
    book = Book(conn)
    #help(Book.select_row)
    #help(Book.insert_row)
    #help(Book.delete_row)
    #help(Book.update_row)
    #help(Book.update_or_insert_row)
