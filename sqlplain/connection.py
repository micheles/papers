import sys, re, threading
from operator import itemgetter
try:
    from collections import namedtuple
except ImportError:
    from sqlplain.namedtuple import namedtuple
from sqlplain.uri import URI
from sqlplain.configurator import configurator

STRING_OR_COMMENT = re.compile("('[^']*'|--.*\n)")

def qmark2pyformat(sql):
    """
    Take a SQL template and replace question marks with pyformat-style
    placeholders (%s), except in strings and comments.
    """
    out = []
    for i, chunk in enumerate(STRING_OR_COMMENT.split(sql)):
        if i % 2 == 0: # real sql code
            out.append(chunk.replace('?', '%s'))
        else: # string or comment
            out.append(chunk)
    return ''.join(out)

class TupleList(list):
    "Used as result of Connection.execute"
    header = None
    rowcount = None
    import re

def _execute(dbtype, cursor, templ, args):
    """
    Call a dbapi2 cursor; return the rowcount or a list of tuples.
    """
    if dbtype == 'sqlite':
        execute = cursor.executescript
    else:
        execute = cursor.execute
    if args:
        execute(templ, args)
    else:
        execute(templ)
    if cursor.description is None: # after an update
        return cursor.rowcount
    else: # after a select
        return cursor.fetchall()

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

class Storage(object):
    "A place where to store low level connection and cursor"

    @classmethod
    def new(cls, connect, args):
        self = cls()
        self.connect = connect
        self.args = args
        self._conn = None
        self._curs = None
        return self
    
    def getconnection(self):
        conn = self._conn
        if conn is None:
            conn = self._conn = self.connect(*self.args)
        return conn
    
    def getcursor(self):
        curs = self._curs
        if curs is None:
            curs = self._curs = self.getconnection().cursor()
        return curs
        
    def close(self):
        """The next time you will call an active method, a fresh new
        connection will be instantiated"""
        if self._curs:
            self._curs.close()
            self._curs = None
        if self._conn:
            self._conn.close()
            self._conn = None
        
class ThreadLocalStorage(threading.local, Storage):
    "A threadlocal object where to store low level connection and cursor"

class Connection(object):
    """
    A lazy callable object returning recordsets. It is lazy since the
    database connection is performed at calling time, not at inizialization
    time.  The connection factory must return (memoized) connections.
    DBI objects can also be used to perform
    actions on the database, via the 'execute' method.
    Notice that this class does not manage any kind of logging, on purpose.
    There easy however a chatty method for easy of debugging.
    """
    
    @classmethod
    def connect(cls, alias, autocommit=True, threadlocal=False):
        return cls(configurator.uri[alias], autocommit, threadlocal)
                   
    def __init__(self, uri, autocommit=True, threadlocal=False):
        self.uri = URI(uri)
        self.dbtype = self.uri['dbtype']
        self.driver, connect, params = self.uri.get_driver_connect_params()
        args = params, autocommit
        self._conn = None
        self.chatty = False
        self.autocommit = autocommit
        self.threadlocal = threadlocal
        if threadlocal:
            self._store = ThreadLocalStorage.new(connect, args)
        else:
            self._store = Storage.new(connect, args)
        if not self.autocommit:
            def rollback(self): return self.conn.rollback()
            def commit(self): return self.conn.commit()
            self.rollback = rollback.__get__(self)
            self.commit = commit.__get__(self)

    def execute(self, templ, args=(), ntuple=None):
        if self.driver.paramstyle == 'pyformat':
            templ = qmark2pyformat(templ)
        cursor = self.curs # make a new connection if needed
        if self.chatty:
            print cursor.rowcount, templ, args
        try:
            res = _execute(self.dbtype, cursor, templ, args)
        except Exception, e:
            tb = sys.exc_info()[2]
            raise e.__class__, '%s\nQUERY WAS:%s%s' % (e, templ, args), tb      
        if isinstance(res, list):
            fields = map(itemgetter(0), cursor.description)
            Ntuple = ntuple or namedtuple('DBTuple', fields)
            res = TupleList(Ntuple(*row) for row in res)
            res.header = Ntuple(*fields)
        return res

    def one(self, templ, args=()):
        return _execute(self.curs, templ, args)[0][0]

    def close(self):
        """The next time you will call an active method, a fresh new
        connection will be instantiated"""
        self._store.close()
        
    def __repr__(self):
        return "<Connection %s, autocommit=%s>" % (self.uri, self.autocommit)

    @property
    def conn(self):
        "Return the low level underlying connection"
        return self._store.getconnection()
   
    @property
    def curs(self):
        "Return the low level underlying cursor"
        return self._store.getcursor()

class PConnection(Connection):
    """
    A persistent connection class for use in multithreaded applications.
    The underlying connections are stored in a threadlocal object, and
    the .execute method has a resetting feature.
    """
    
    ## execute a query, by retrying it once when losing connection
    def execute(self, templ, args=(), ntuple=None):
        execute = super(PConnection, self).execute
        try:
            return execute(templ, args, ntuple)
        except self.driver.OperationalError, e: # missing connection
            self.close() # print e, 'resetting connection ...'
            return execute(templ, args, ntuple)

def openclose(uri, templ, *args, **kw):
    "Open a connection, perform an action and close the connection"
    unexpected = set(kw) - set(['autocommit'])
    if unexpected:
        raise ValueError('Received unexpected keywords: %s' % unexpected)
    autocommit = kw.get('autocommit', True)
    conn = Connection(uri, autocommit)
    try:
        if autocommit:
            return conn.execute(templ, args)
        else:
            return transact(Connection.execute, conn, templ, args)
    finally:
        conn.close()
