import sys, re, threading
from operator import itemgetter
try:
    from collections import namedtuple
except ImportError:
    from sqlplain.namedtuple import namedtuple
from sqlplain.uri import URI

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
        self.errors = (self.driver.OperationalError,
                       self.driver.ProgrammingError,
                       self.driver.InterfaceError)
        
    def _execute(self, cursor, templ, args):
        """
        Call a dbapi2 cursor; return the rowcount or a list of tuples.
        """
        if self.driver.paramstyle == 'pyformat':
            templ = qmark2pyformat(templ)
        try:
            if args:
                cursor.execute(templ, args)
            #elif self.dbtype == 'sqlite':
            #    cursor.executescript(templ)
            else:
                cursor.execute(templ)
        except Exception, e:
            tb = sys.exc_info()[2]
            raise e.__class__, '%s\nQUERY WAS:%s%s' % (e, templ, args), tb    
        if cursor.description is None: # after an update
            return cursor.rowcount
        else: # after a select
            return cursor.fetchall()

    def execute(self, templ, args=(), ntuple=None):
        cursor = self.curs # make a new connection if needed
        if self.chatty:
            print cursor.rowcount, templ, args
        res = self._execute(cursor, templ, args)    
        if isinstance(res, list):
            fields = map(itemgetter(0), cursor.description)
            if ntuple is None:
                Ntuple = namedtuple('DBTuple', fields)
            elif isinstance(ntuple, str):
                Ntuple = namedtuple(ntuple, fields)
            else:
                Ntuple = ntuple
            res = TupleList(Ntuple(*row) for row in res)
            res.header = Ntuple(*fields)
        return res

    def getone(self, templ, args=()):
        rows = self._execute(self.curs, templ, args)
        if len(rows) != 1 or len(rows[0]) != 1:
            raise ValueError("Expected to get a singleton result, got %s"
                             % rows)
        return rows[0][0]

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
    
    ## _execute a query, by retrying it once when losing connection
    def _execute(self, cursor, templ, args):
        _execute = super(PConnection, self)._execute
        try:
            return _execute(cursor, templ, args)
        except self.errors, e: # missing connection
            print e, 'resetting connection ...'
            self.close()
            return _execute(cursor, templ, args)

### utility functions
 
def do(templ):
    def _do(conn, *args):
        return conn.execute(templ, args)
    _do.__name__ = templ
    return _do


def getone(templ):
    def _getone(conn, *args):
        return conn.getone(templ, args)
    _getone.__name__ = templ
    return _getone
