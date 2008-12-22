import sys, string
from sqlplain import util
from sqlplain.namedtuple import namedtuple
from sqlplain.connection import connmethod


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

# note: the methods of the Table classes
# are completely external to the class except for the .conn attribute

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

class DTable(object):
    """
    A simple table class for database tables without a primary key.
    The only methods are insert_row, insert_file, delete, truncate, select.
    """
    _registry = {}
    
    @classmethod
    def type(cls, name, fields):
        "Ex. Insert = DTable.type('book', 'serial', 'title author')"
        fields = tuple(fields)
        if (name, fields) in cls._registry:
            return cls._registry[name, fields]
        tt = namedtuple(name, fields)
        def insert_file(conn, file, sep='\t'):
            'Populate a table by reading a file-like object'
            return util.insert_file(conn, file, name, sep)
        def insert_rows(conn, rows):
            'Populate a table by reading a row-iterator'
            return util.insert_rows(conn, name, rows)
        dic = dict(
            tt = tt,
            name = tt.__name__,
            insert_row = connmethod(insert(tt)),
            insert_rows = connmethod(insert_rows),
            insert_file = connmethod(insert_file),
            _registry = {},
            )
        subc = type(name.capitalize(), (cls,), dic)
        cls._registry[name, fields] = subc
        return subc

    @classmethod
    def reflect(cls, conn, name):
        "Ex. insert = DTable.reflect(mydb, 'book')"
        fields = util.get_fields(conn, name)
        return cls.type(name, fields)(conn)

    @connmethod
    def insert_row(conn, row, **kw):
        "Dynamically replaced in subclasses"

    @connmethod
    def insert_rows(conn, row, **kw):
        "Dynamically replaced in subclasses"

    @connmethod
    def insert_file(conn, row, **kw):
        "Dynamically replaced in subclasses"

    def __init__(self, conn):
        if self.__class__ in (DTable, KTable):
            raise TypeError('You cannot instantiate the ABC %s' %
                            self.__class__.__name__)
        self.tt # raise an AttributeError if not set correctly
        self.conn = conn

    def select(self, clause=''):
        "Select rows from the table"
        fields = ', '.join(self.tt._fields)
        return self.conn.execute(
            'SELECT %s FROM %s %s' % (fields, self.name, clause),
            ntuple=self.tt)

    def delete(self, clause=''):
        "Delete rows from the table"
        return self.conn.execute('DELETE FROM ' + self.name + ' ' + clause)

    def truncate(self):
        "Truncate the table"
        return util.truncate_table(self.conn, self.name)

    def count(self, clause=''):
        "Count the number of rows satisfying the given clause"
        return self.conn.execute(
            'SELECT count(*) FROM %s %s' % (self.name, clause), scalar=True)

    def __len__(self):
        "Return the total number of rows in the table"
        return self.count()
    
class KTable(DTable):
    """
    An object oriented wrapper for database tables with a primary key.
    """
    _registry = {}
    
    @classmethod
    def type(cls, name, kfields, dfields):
        "Ex. Book = KTable.type('book', 'serial', 'title author')"
        if not kfields:
            raise TypeError('table %s has no primary key!' % name)
        kfields = tuple(kfields)
        dfields = tuple(dfields)
        if (name, kfields, dfields) in cls._registry:
            return cls._registry[name, kfields, dfields]
        tt = tabletuple(name, kfields, dfields)
        d = dict(tt=tt, name=name, _registry = {})
        for nam in ('insert', 'delete', 'select', 'update', 'update_or_insert'):
            func = globals()[nam](tt)
            cmethod = connmethod(func)
            d[nam + '_row'] = cmethod
        subc = type(name.capitalize(), (cls,), d)
        cls._registry[name, kfields, dfields] = subc
        return subc

    @classmethod
    def reflect(cls, conn, name):
        "Ex. book = KTable.reflect(mydb, 'book')"
        kfields = util.get_kfields(conn, name)
        dfields = util.get_dfields(conn, name)
        return cls.type(name, kfields, dfields)(conn)

    def __contains__(self, key):
        pass
    
    def keys(self):
        kfields = ', '.join(self.tt._kfields)
        return self.conn.execute(
            'SELECT %s FROM %s' % (kfields, self.name))

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
