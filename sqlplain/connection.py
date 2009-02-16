import sys, threading, itertools, string
from operator import attrgetter
try:
    from collections import namedtuple
except ImportError:
    from sqlplain.namedtuple import namedtuple
from sqlplain.uri import URI
from sqlplain.sql_support import qmark2pyformat
from decorator import decorator

@decorator
def retry(func, conn, *args, **kw):
    """
    A decorator making a function taking a connection as first argument
    to retry in case of errors
    """
    if conn.retry:
        try:
            return func(conn, *args, **kw)
        except conn.driver.Error, e:            
            conn.close() # retry with a fresh connection
            return func(conn, *args, **kw)
    else:
        return func(conn, *args, **kw)

Field = namedtuple(
    'Field',
    'name type_code display_size internal_size precision scale null_ok')

counter = itertools.count(1)

def noname():
    return 'noname%d' % counter.next()

class TupleList(list):
    "Used as result of LazyConn.execute"
    header = None
    rowcount = None
    descr = None

class Transaction(object):
    """
    A wrapper around database actions with signature (conn, *args, **kw).
    """
    def __init__(self, action, conn, *args, **kw):
        self.action = action
        self.conn = conn
        self.args = args
        self.kw = kw

    def run(self, conn=None, args=None, kw=None, commit=True):
        "Execute the action in a transaction"
        conn = conn or self.conn
        args = args or self.args
        kw = kw or self.kw
        try:
            return self.action(conn, *args, **kw)
        except Exception, e:
            conn.rollback()
            raise e.__class__, e, sys.exc_info()[2]
        else:
            if commit:
                conn.commit()
            else:
                conn.rollback()

class _Storage(object):
    "A place where to store the low level connection and cursor"

    @classmethod
    def new(cls, connect, args):
        "Make a subclass of _Storage and instantiate it"
        subc = type('%sSubclass' % cls.__name__, (cls,),
                    dict(connect=staticmethod(connect), args=args))
        return subc()
    
    @property
    def conn(self):
        "Return the low level connection"
        conn = getattr(self, '_conn', None)
        if conn is None:
            connect, args = self.__class__.connect, self.__class__.args
            conn = self._conn = connect(*args)
        return conn

    @property
    def curs(self):
        "Return the low level cursor"
        curs = getattr(self, '_curs', None)
        if curs is None:
            curs = self._curs = self.conn.cursor()
        return curs
        
    def close(self):
        """The next time you will call an active method, a fresh new
        connection will be instantiated"""
        if getattr(self, '_curs', False):
            try:
                self._curs.close()
            except: # ignore if already closed
                pass
            self._curs = None
        if getattr(self, '_conn', False):
            try:
                self._conn.close()
            except: # ignore if already closed
                pass
            self._conn = None
        
class _ThreadLocalStorage(threading.local, _Storage):
    "A threadlocal object where to store low level connection and cursor"

class LazyConnection(object):
    """
    A lazy connection object. It is lazy since the database connection
    is performed at execution time, not at inizialization time. Notice
    that this class does not manage any kind of logging, on purpose.
    There is however a chatty method for easy of debugging.
    """    
    def __init__(self, uri, isolation_level=None, threadlocal=False,
                 retry=False):
        self.uri = URI(uri)
        self.name = self.uri['database']
        self.dbtype = self.uri['dbtype']
        self.driver, connect, params = self.uri.get_driver_connect_params()
        args = params, isolation_level
        self.chatty = False
        self.isolation_level = isolation_level
        self.threadlocal = threadlocal
        if threadlocal:
            self._storage = _ThreadLocalStorage.new(connect, args)
        else:
            self._storage = _Storage.new(connect, args)
        self.retry = retry

    @retry
    def _raw_execute(self, templ, args):
        """
        Call a dbapi2 cursor; return the rowcount or a list of tuples,
        plus an header (None in the case of the rowcount).
        """
        cursor = self._storage.curs
        try:
            cursor.execute(templ, args)
            #if args:
            #    cursor.execute(templ, args)
            #else:
            #    cursor.execute(templ)
        except Exception, e:
            tb = sys.exc_info()[2]
            raise e.__class__, '%s\nQUERY WAS:%s%s' % (e, templ, args), tb
        descr = cursor.description
        if descr is None: # after an update
            return None, cursor.rowcount
        else: # after a select
            return descr, cursor.fetchall()

    def execute(self, templ, args=(), ntuple=None, scalar=False):
        if self.dbtype == 'mssql':
            # converts unicode arguments to utf8
            lst = []
            for a in args:
                if isinstance(a, unicode):
                    lst.append(a.encode('utf8'))
                else:
                    lst.append(a)
            args = tuple(lst)
                              
        if self.driver.paramstyle == 'pyformat':
            qmarks, templ = qmark2pyformat(templ) # cached
            if qmarks != len(args): # especially useful for mssql
                raise TypeError("Expected %d arguments, got %d: %s" % (
                    qmarks, len(args), args))
        
        descr, res = self._raw_execute(templ, args)
        if scalar: # you expect a scalar result
            if not res:
                raise KeyError(
                    "Missing record, QUERY WAS:%s%s\n" % (templ, args))
            elif len(res) > 1:
                raise ValueError(
                    "Expected to get a scalar result, got %s\nQUERY WAS:%s%s\n"
                    % (res, templ, args))
            return res[0][0]
        cursor = self._storage.curs # needed to make the reset work
        if self.chatty:
            print(cursor.rowcount, templ, args)
        if descr: # the query was a SELECT
            fields = [Field(*d) for d in descr]
            header = [f.name or noname() for f in fields]
            if ntuple is None:
                Ntuple = namedtuple('DBTuple', header)
            elif isinstance(ntuple, str):
                Ntuple = namedtuple(ntuple, header)
            else:
                Ntuple = ntuple
            res = TupleList(Ntuple(*row) for row in res)
            res.descr = fields
            res.header = Ntuple(*header)
        return res

    def executescript(self, sql, *dicts, **kw):
        "A driver-independent method to execute sql templates"
        d = {}
        for dct in dicts + (kw,):
            d.update(dct)
        if d:
            sql = string.Template(sql).substitute(d) 
        if self.dbtype == 'sqlite':
            self._storage.curs.executescript(sql)
        else: # psycopg and pymssql are already able to execute chunks
            self.execute(sql)

    def open(self):
        "Return the low level underlying connection"
        return self._storage.conn
            
    def close(self):
        """The next time you will call an active method, a fresh new
        connection will be instantiated"""
        self._storage.close()
        
    def __repr__(self):
        return "<%s %s>" % (self.__class__.__name__, self.uri)
    
    @property
    def rowcount(self):
        return self._storage.curs.rowcount

class TransactionalConnection(LazyConnection):
    """
    Add commit and rollback methods to a LazyConnection, as well
    as a with statement interface.
    """

    def rollback(self):
        return self._storage.conn.rollback()

    def commit(self):
        return self._storage.conn.commit()

    def __enter__(self):
        return self

    def __exit__(self, exc_class, exc, tb):
        if exc_class:
            self.rollback()
            raise exc_class, exc, tb
        else:
            self.commit()
        
    def __repr__(self):
        return "<%s %s, isolation_level=%s>" % (
            self.__class__.__name__, self.uri, self.isolation_level)

class NullObject(object):
    '''Implements the NullObject pattern.
    
    >>> n = NullObject()
    >>> n.dosomething(1,2,3)
    '''
    def __getattr__(self, name):
        return lambda *a, **k: None
    def __repr__(self):
        return 'None'
    def __nonzero__(self):
        return False
    def __iter__(self):
        return ()
    def __call__(self, *a, **k):
        return None

class FakeConnection(object):
    def __init__(self, iodict):
        self.iodict = iodict
        self._storage = NullObject()
    def execute(self, templ, args=()):
        return self.iodict[(templ,) + args]
    def executescript(self, templ, *dicts, **kw):
        pass
    def commit(self):
        pass
    def rollback(self):
        pass
    def close(self):
        pass
    def __enter__(self):
        return self
    def __exit_(self, exctype=None, exc=None, tb=None):
        pass
