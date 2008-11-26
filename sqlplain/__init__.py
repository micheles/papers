from sqlplain.connection import LazyConn, transact, dry_run

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
