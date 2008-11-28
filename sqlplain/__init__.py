from sqlplain.connection import LazyConnection, TransactionalConnection, \
     transact, dry_run

def lazyconnect(uri, isolation_level=None, threadlocal=False, conn_class=None):
    if conn_class is None and isolation_level is None:
        conn_class = LazyConnection
    elif conn_class is None and isolation_level is not None:
        conn_class = TransactionalConnection        
    return conn_class(uri, isolation_level, threadlocal)
    
def inspect(db, name):
    return db.execute('SELECT * FROM %s WHERE 1=0;' % name, ntuple=name).header

### utility functions

def do(templ):
    """
    Wrap a query template. Return a closure with arguments
    (dbconn, *query_args).
    """
    def _do(conn, *args):
        return conn.execute(templ, args)
    _do.__name__ = templ
    return _do

def _one(templ):
    """
    Wrap a query template which is expected to return a scalar result.
    Raise a ValueError otherwise.
    """
    def _getone(conn, *args):
        return conn.getone(templ, args)
    _getone.__name__ = templ
    return _getone

do.one = _one
