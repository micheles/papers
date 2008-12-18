import sys, threading, itertools, string
from operator import attrgetter
try:
    from collections import namedtuple
except ImportError:
    from sqlplain.namedtuple import namedtuple
from sqlplain.uri import URI
from sqlplain.sql_support import qmark2pyformat

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
    
def transact(action, conn, *args, **kw):
    "Run a function in a transaction"
    try:
        return action(conn, *args, **kw)
    except Exception, e:
        conn.rollback()
        raise e.__class__, e, sys.exc_info()[2]
    else:
        conn.commit()

def dry_run(action, conn, *args, **kw):
    "Run a function in a transaction and rollback everything at the end"
    try:
        return action(conn, *args, **kw)
    finally:
        conn.rollback()

class _Storage(object):
    "A place where to store low level connection and cursor"

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

    retry = False
    
    def __init__(self, uri, isolation_level=None, threadlocal=False):
        self.uri = URI(uri)
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
        self.errors = (self.driver.OperationalError,
                       self.driver.ProgrammingError,
                       self.driver.InterfaceError,
                       self.driver.DatabaseError)
        
    def _raw_execute(self, cursor, templ, args):
        """
        Call a dbapi2 cursor; return the rowcount or a list of tuples,
        plus an header (None in the case of the rowcount).
        """
        try:
            if args:
                cursor.execute(templ, args)
            else:
                cursor.execute(templ)
        except Exception, e:
            tb = sys.exc_info()[2]
            raise e.__class__, '%s\nQUERY WAS:%s%s' % (e, templ, args), tb
        descr = cursor.description
        if descr is None: # after an update
            return None, cursor.rowcount
        else: # after a select
            return descr, cursor.fetchall()

    def _execute(self, cursor, templ, args):
        """
        _raw_execute a query, by retrying it once with a fresh connection
        in case of error if the .retry flag is set.
        """
        raw_execute = self._raw_execute
        if not self.retry:
            return raw_execute(cursor, templ, args)
        try:
            return raw_execute(cursor, templ, args)
        except self.errors, e: # maybe bad connection
            self.close() # reset connection and retry
            print '%s, retrying' % e
            return raw_execute(self._curs, templ, args)

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
        
        descr, res = self._execute(self._curs, templ, args)
        if scalar: # you expect a scalar result
            if len(res) != 1 or len(res[0]) != 1:
                raise ValueError(
                    "Expected to get a scalar result, got %s\nQUERY WAS:%s%s\n"
                    % (res, templ, args))
            return res[0][0]
        cursor = self._curs # needed to make the reset work
        if self.chatty:
            print(cursor.rowcount, templ, args)
        if descr:
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
            self._curs.executescript(sql)
        else: # psycopg and pymssql are already able to execute chunks
            self.execute(sql)
            
    def close(self):
        """The next time you will call an active method, a fresh new
        connection will be instantiated"""
        self._storage.close()
        
    def __repr__(self):
        return "<%s %s>" % (self.__class__.__name__, self.uri)

    @property
    def _conn(self):
        "Return the low level underlying connection"
        return self._storage.conn
   
    @property
    def _curs(self):
        "Return the low level underlying cursor"
        return self._storage.curs
    
class TransactionalConnection(LazyConnection):
    """
    Add commit and rollback methods to a LazyConnection, as well
    as a with statement interface.
    """

    def rollback(self):
        return self._conn.rollback()

    def commit(self):
        return self._conn.commit()

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
        self._conn = NullObject()
        self._curs = NullObject()
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
    def __exit_(self, exctype, exc, tb):
        pass

class connmethod(object):
    """
    A descriptor for methods which first argument is a (lazy) connection.
    Used to decorate methods of classes with a .conn attribute.
    """
    def __init__(self, func):
        self._func = func
        self.__name__ = func.__name__
        #self.__doc__ = func.__doc__
        #self.__dict__ = func.__dict__
        #self.__module__ = func.__module__
    def __get__(self, obj, objcls):
        if obj is None: # called from the class
            return self._func
        else: # called from the instance
            return self._func.__get__(obj.conn, objcls)
