import sys, UserDict
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
    _execute.__name__ = '%s_select' % ttuple.__name__
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
    _execute.__name__ = '%s_select' % ttuple.__name__
    _execute.__doc__ = templ
    _execute.__module__ = sys._getframe(2).f_globals['__name__']
    return _execute

# used only in update and insert
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
        return conn.execute(templ, row,)
    _execute.__name__ = '%s_%s' % (ttuple.__name__, name)
    _execute.__doc__ = templ
    _execute.__module__ = sys._getframe(2).f_globals['__name__']
    return _execute

def insert(ttuple):
    "Return a procedure inserting a row or a dictionary into a table"
    name = ttuple.__name__
    csfields = ','.join(ttuple._fields)
    qmarks = ','.join('?'*len(ttuple._fields))
    templ = 'INSERT INTO %s (%s) VALUES (%s)' % (name, csfields, qmarks)
    return _updater(ttuple, templ, 'insert')

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
    up_or_ins.__name__ = '%s_update_or_insert' % ttuple.__name__
    return up_or_ins

for factory in select, delete, insert, update, update_or_insert:
    def row(name, kfields, dfields='', factory=factory):
        return factory(tabletuple(name, kfields, dfields))
    name = '%s_row' % factory.__name__
    row.__name__ = name
    globals()[name] = row
    # insert_book = insert_row('book', 'title author')
    # insert_book(conn, title='T', author='A')
    # update_row('book', 'title author', 'pubdate')
    # delete_row('book', 'title author')
    
def table(name, kfields, dfields):
    _tt = tabletuple(name, kfields, dfields)
    _select = select(_tt)
    _insert = insert(_tt)
    _delete = delete(_tt)
    _update = update(_tt)
    _update_or_insert = update_or_insert(_tt)
    
    class Table(UserDict.DictMixin):
        tuplecls = _tt
        def __init__(self, conn):
            self.conn = conn
        def __getitem__(self, key):
            return self.select(key)
        def __setitem__(self, key, val):
            self.update_or_insert(val + key)
        def keys(self):
            kfields = ', '.join(_tt._kfields)
            return self.conn.execute('SELECT %s FROM %s' % (kfields, name))
        def insert(self, row=None, **kw):
            return _insert(self.conn, row, **kw)
        def delete(self, row=None, **kw):
            return _delete(self.conn, row, **kw)
        def select(self, row=None, **kw):
            return _select(self.conn, row, **kw)
        def update(self, row=None, **kw):
            return _update(self.conn, row, **kw)
        def update_or_insert(self, row=None, **kw):
            return _update_or_insert(self.conn, row, **kw)

    return Table

# book = Table('book', 'title author', 'pubdate')(conn)
# book.select(author='A')
# book.insert(title='T', author='A', pubdate='P')
# book.update(title='T', author='A', pubdate='P')
# or insert('book', 'title author')(conn, title="T", author="A")

if __name__ == '__main__':
    tt = tabletuple('tt', 'x y', 'a,b')(1, 2, 3, 4)
    print tt._kvalues, tt._dvalues
    print tt.__class__.mro()

    b = Table('book', 'pubdate', 'title author')
    help(b.select)
    help(b.insert)
    help(b.delete)
    help(b.update)
    help(b.update_or_insert)
