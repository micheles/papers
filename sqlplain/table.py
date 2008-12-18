import sys
from sqlplain import util
from sqlplain.namedtuple import namedtuple

def tabletuple(name, kfields, dfields):
    """
    Returns a namedtuple with attributes ._kfields, ._dfields and properties
    ._kvalues, .dvalues. This is needed to send records to a database table
    with a primary key
    """
    ttuple = namedtuple(name, kfields + ' ' + dfields)
    ttuple._ktuple = ktuple = namedtuple(name + '_key', kfields)
    ttuple._dtuple = dtuple = namedtuple(name + '_data', dfields)
    ttuple._kfields = ktuple._fields
    ttuple._dfields = dtuple._fields
    ttuple._kvalues = property(
        lambda self: ktuple(*[getattr(self, n) for n in self._kfields]))
    ttuple._dvalues = property(
        lambda self: dtuple(*[getattr(self, n) for n in self._dfields]))
    return ttuple

def select(ttuple):
    """
    Returns a function with signature (conn, key) - where key can be
    a dictionary or a tabletuple - returning a single row.
    """
    name = ttuple.__name__
    csfields = ','.join(ttuple._fields)
    clause = ' AND '.join('%s=?' % field for field in ttuple._kfields)
    templ = 'SELECT %s FROM %s WHERE %s' % (csfields, name, clause)
    def _execute(conn, row=None, **kw):
        row = row or {}
        if isinstance(row, dict):
            row.update(kw)
            row = ttuple._ktuple(**row)        
        res = conn.execute(templ, row, ttuple)
        if len(res) != 1:
            raise KeyError('Got %s instead of a single row' % res)
        return res[0]
    _execute.__name__ = '_%s_select' % ttuple.__name__
    _execute.__doc__ = templ
    _execute.__module__ = sys._getframe(2).f_globals['__name__']
    return _execute

def delete(ttuple):
    "Return a procedure inserting a row or a dictionary into a table"
    name = ttuple.__name__
    clause = ' AND '.join('%s=?' % field for field in ttuple._kfields)
    templ = 'DELETE FROM %s WHERE %s' % (name, clause)
    def _execute(conn, row=None, **kw):
        row = row or {}
        if isinstance(row, dict):
            row.update(kw)
            row = ttuple._ktuple(**row)        
        return conn.execute(templ, row, ttuple)
    _execute.__name__ = '_%s_delete' % ttuple.__name__
    _execute.__doc__ = templ
    _execute.__module__ = sys._getframe(2).f_globals['__name__']
    return _execute

# used only in update
def _updater(ttuple, templ, name):
    """
    Returns a function with signature (conn, row) where row can be
    a dictionary or a namedtuple/tabletuple.
    """
    def _execute(conn, row=None, **kw):
        row = row or {}
        if isinstance(row, dict):
            row.update(kw)
            row = ttuple(**row)
        return conn.execute(templ, row._dvalues + row._kvalues)
    _execute.__name__ = '_%s_%s' % (ttuple.__name__, name)
    _execute.__doc__ = templ
    _execute.__module__ = sys._getframe(2).f_globals['__name__']
    return _execute

# used only in insert
def _inserter(ttuple, templ, name):
    """
    Returns a function with signature (conn, row) where row can be
    a dictionary or a namedtuple/tabletuple.
    """
    def _execute(conn, row=None, **kw):
        row = row or {}
        if isinstance(row, dict):
            row.update(kw)
            row = ttuple(**row)
        return conn.execute(templ, row)
    _execute.__name__ = '_%s_%s' % (ttuple.__name__, name)
    _execute.__doc__ = templ
    _execute.__module__ = sys._getframe(2).f_globals['__name__']
    return _execute

def insert(ttuple):
    "Return a procedure inserting a row or a dictionary into a table"
    name = ttuple.__name__
    csfields = ','.join(ttuple._fields)
    qmarks = ','.join('?'*len(ttuple._fields))
    templ = 'INSERT INTO %s (%s) VALUES (%s)' % (name, csfields, qmarks)
    return _inserter(ttuple, templ, 'insert')

def update(ttuple):
    "Returns a procedure updating a row"
    name = ttuple.__name__
    set = ', '.join('%s=?' % field for field in ttuple._dfields)
    where = ' AND '.join('%s=?' % field for field in ttuple._kfields)
    templ = 'UPDATE %s SET %s WHERE %s' % (name, set, where)
    return _updater(ttuple, templ, 'update')

def update_or_insert(ttuple):
    "Returns a procedure updating or inserting a row"
    up = update(ttuple)
    ins = insert(ttuple)
    def up_or_ins(conn, row=None, **kw):
        n = up(conn, row, **kw)
        if n == 0:
            n = ins(conn, row, **kw)
        return n
    up_or_ins.__name__ = '_%s_update_or_insert' % ttuple.__name__
    return up_or_ins

#for factory in select, delete, insert, update, update_or_insert:
#    def row(name, kfields, dfields='', factory=factory):
#        return factory(tabletuple(name, kfields, dfields))
#    name = '%s_row' % factory.__name__
#    row.__name__ = name
#    globals()[name] = row
    # insert_book = insert_row('book', 'title author')
    # insert_book(conn, title='T', author='A')
    # update_row('book', 'title author', 'pubdate')
    # delete_row('book', 'title author')

class Table(object):
    @classmethod
    def type_(cls, name, kfields, dfields):
        "Ex. Book = Table.type_('book', 'serial', 'title author')"
        tt = tabletuple(name, kfields, dfields)
        d = dict(tt=tt)
        for nam in ('insert', 'delete', 'select', 'update', 'update_or_insert'):
            methodname = nam + '_row'
            privatemethod = globals()[nam](tt)
            privatename = privatemethod.__name__
            d[privatename] = staticmethod(privatemethod)
            d['%s_templ' % name] = privatemethod.__doc__
        return type(name.capitalize(), (cls,), d)

    @classmethod
    def from_(cls, conn, name):
        "Ex. book = Table.from_(mydb, 'book')"
        kfields = util.get_kfields(conn, name)
        dfields = util.get_dfields(conn, name)
        return cls.type_(name, kfields, dfields)(conn)

    def __init__(self, conn):
        self.tt # raise AttributeError if not initialized correctly
        self.conn = conn

    def __contains__(self, key):
        pass
        
    def keys(self):
        kfields = ', '.join(self.tt._kfields)
        return self.conn.execute(
            'SELECT %s FROM %s' % (kfields, name))

    def allrows(self, clause=''):
        fields = ', '.join(self.tt._fields)
        return self.conn.execute(
            'SELECT %s FROM %s %s' % (fields, name, clause), ntuple=self.tt)

    # keep the following five methods explicit for inspectability purposes
    
    def insert_row(self, row=None, **kw):
        return self._insert_row(self.conn, row, **kw)

    def delete_row(self, row=None, **kw):
        return self._delete_row(self.conn, row, **kw)

    def select_row(self, row=None, **kw):
        return self._select_row(self.conn, row, **kw)

    def update_row(self, row=None, **kw):
        return self._update_row(self.conn, row, **kw)

    def update_or_insert_row(self, row=None, **kw):
        return self._update_or_insert_row(self.conn, row, **kw)

# book = Table('book', 'title author', 'pubdate')(conn)
# book.select(author='A')
# book.insert(title='T', author='A', pubdate='P')
# book.update(title='T', author='A', pubdate='P')
# or insert('book', 'title author')(conn, title="T", author="A")

if __name__ == '__main__':
    tt = tabletuple('tt', 'x y', 'a,b')(1, 2, 3, 4)
    print tt._kvalues, tt._dvalues
    print tt.__class__.mro()

    Book = Table.type('book', 'pubdate', 'title author')
    help(Book._book_select)
    #help(Book.select_row)
    #help(Book.insert_row)
    #help(Book.delete_row)
    #help(Book.update_row)
    #help(Book.update_or_insert_row)
