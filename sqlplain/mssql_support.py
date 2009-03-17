import sys
import _mssql
import pymssql as dbapi2
from pymssql import OperationalError, pymssqlCursor as Cursor

ISOLATION_LEVELS = (
    None, 'READ UNCOMMITTED', 'READ COMMITTED', 'REPEATABLE READ',
    'SERIALIZABLE')

placeholder = '%s'

class Connection(object):

    def __init__(self, cnx, isolation_level=None):
       assert isolation_level in ISOLATION_LEVELS, isolation_level
       self._cnx = cnx
       self.isolation_level = isolation_level
       if isolation_level:
           cnx.query("set transaction isolation level " + isolation_level)
           try:
               self._cnx.query("begin tran")
               self._cnx.fetch_array()
           except Exception, e:
               raise OperationalError(e)

    def cursor(self):
        if self._cnx is None:
            raise OperationalError("Closed connection")
        cursor = Cursor(self._cnx)
        if not hasattr(cursor, 'connection'): # for old versions of pymssql
            cursor.connection = self
        return cursor
    
    def close(self):
        if self._cnx is None:
            return # the connection was already closed
        self._cnx.close()
        self._cnx = None
        
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

def connect(params, isolation_level=None):
    user, pwd, host, port, db = params
    port = port or 1433
    if sys.platform != 'win32': # on linux 
        if '\\' in host: # strip the instance
            host, instance = host.split('\\')
        host = '%s:%s' % (host, port) # add the port
    _conn = _mssql.connect(host, user, pwd)
    _conn.select_db(db)
    return Connection(_conn, isolation_level)
