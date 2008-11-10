import sys
import _mssql
from pymssql import OperationalError, pymssqlCursor as Cursor

class Connection(object):

    def __init__(self, cnx):
       self._cnx = cnx

    def cursor(self):
        if self._cnx is None:
            raise OperationalError("Closed connection")
        cursor = Cursor(self._cnx)
        cursor.connection = self
        return cursor
    
    def close(self):
        if self._cnx is None:
            return # the connection was already closed
        self._cnx.close()
        self._cnx = None

class TransactionalConnection(Connection):
    
    def __init__(self, cnx):
        self._cnx = cnx
        try:
            self._cnx.query("begin tran")
            self._cnx.fetch_array()
        except Exception, e:
            raise OperationalError(e)
        
    def commit(self):
        if self._cnx is None:
            raise OperationalError("Closed connection")
        try:
            self._cnx.query("commit tran")
            self._cnx.fetch_array()
            self._cnx.query("begin tran")
            self._cnx.fetch_array()
        except Exception, e:
            raise OperationalError("can't commit: %s" % e)

    def rollback(self):
        if self._cnx is None:
            raise OperationalError("Closed connection")
        try:
            self._cnx.query("rollback tran")
            self._cnx.fetch_array()
            self._cnx.query("begin tran")
            self._cnx.fetch_array()
        except Exception, e:
            raise OperationalError("can't rollback: %s" % e)
        
class AutoCommitConnection(Connection):

    def commit(self):
        raise NotImplementedError('You are in auto_commit mode!')

    def rollback(self):
        raise NotImplementedError('You are in auto_commit mode!')

def connect(params, autocommit=True):
    user, pwd, host, port, db = params
    port = port or 1433
    if sys.platform != 'win32': # on linux 
        if '\\' in host: # strip the instance
            host, instance = host.split('\\')
        host = '%s:%s' % (host, port) # add the port
    _conn = _mssql.connect(host, user, pwd)
    _conn.select_db(db)
    if autocommit:
        conn = AutoCommitConnection(_conn)
    else:
        conn = TransactionalConnection(_conn)
    return conn

