import sys
from sqlplain.namedtuple import namedtuple

def tabletuple(name, dfields, kfields):
    """
    Returns a namedtuple with attributes ._kfields, ._dfields and properties
    ._kvalues, .dvalues. This is needed to send records to a database table
    with a primary key
    """
    ttuple = namedtuple(name, dfields + ' ' + kfields)
    ktuple = namedtuple(name + '_key', kfields)
    dtuple = namedtuple(name + '_data', dfields)
    ttuple._kfields = ktuple._fields
    ttuple._dfields = dtuple._fields
    ttuple._kvalues = property(
        lambda self: ktuple(*[getattr(self, n) for n in self._kfields]))
    ttuple._dvalues = property(
        lambda self: dtuple(*[getattr(self, n) for n in self._dfields]))
    return ttuple

# used only in select, insert/delete, update/update_or_insert
def queryfn(ttuple, templ, name):
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
    _execute.__name__ = '%s_%s' % (ttuple.__name__, name)
    _execute.__doc__ = templ
    _execute.__module__ = sys._getframe(2).f_globals['__name__']
    return _execute    

def select(ttuple):
    name = ttuple.__name__
    csfields = ','.join(ttuple._dfields)
    clause = ' '.join('AND %s=?' % field for field in ttuple._kfields)
    templ = 'SELECT %s FROM %s' % (name, csfields)
    if clause:
        templ += ' WHERE 1=1 ' + clause
    return queryfn(ttuple, templ, 'select')

def insert(ttuple):
    "Return a procedure inserting a row or a dictionary into a table"
    name = ttuple.__name__
    csfields = ','.join(ttuple._fields)
    qmarks = ','.join('?'*len(ttuple._fields))
    templ = 'INSERT INTO %s (%s) VALUES (%s)' % (name, csfields, qmarks)
    return queryfn(ttuple, templ, 'insert')

def delete(ttuple):
    "Return a procedure inserting a row or a dictionary into a table"
    name = ttuple.__name__
    clause = ' '.join('AND %s=?' % field for field in ttuple._fields)
    templ = 'DELETE FROM %s ' % name
    if clause:
        templ += 'WHERE 1=1 ' + clause
    return queryfn(ttuple, templ, 'delete')

def update(ttuple):
    "Returns a procedure updating a row"
    name = ttuple.__name__
    set = ', '.join('%s=?' % field for field in ttuple._dfields)
    where = ' '.join('AND %s=?' % field for field in ttuple._kfields)
    templ = 'UPDATE %s SET %s WHERE 1=1 %s' % (name, set, where)
    return queryfn(ttuple, templ, 'update')

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

## probably remove this
class Table(object):
    def __init__(self, name, dfields, kfields=''):
        self.tuplecls = tt = tabletuple(name, dfields, kfields)
        self.select = select(tt)
        self.insert = insert(tt)
        self.delete = delete(tt)
        self.update = update(tt)
        self.update_or_insert = update_or_insert(tt)

# book = Table('book', kfields='title author', dfields='pubdate')
# book.select(conn, author='A')
# book.insert(conn, title='T', author='A', pubdate='P')
# book.insert(conn, title='T', author='A', pubdate='P')
# or insert('book', 'title author')(title="T", author="A")

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
