from sqlplain.connection import LazyConnection, TransactionalConnection, \
     transact, dry_run, qmark2pyformat
from sqlplain.sql_support import do

def lazyconnect(uri, isolation_level=None, threadlocal=False, conn_class=None):
    if conn_class is None and isolation_level is None:
        conn_class = LazyConnection
    elif conn_class is None and isolation_level is not None:
        conn_class = TransactionalConnection        
    return conn_class(uri, isolation_level, threadlocal)
