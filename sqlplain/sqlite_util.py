import os
from sqlplain.util import openclose

def get_info(conn, tname):
    """
    Returns a list of namedtuples [(cid, name, type, notnull, dflt_value, pk)]
    """
    return conn.execute('PRAGMA table_info(%s)' % tname)

def get_kfields_sqlite(conn, tname):
    return [x.name for x in get_info(conn, tname) if x.pk]

def exists_table_sqlite(conn, tname):
    res = conn.execute('PRAGMA table_info(%s)' % tname)
    return len(res)

def exists_db_sqlite(uri):
    fname = uri['database']
    return fname == ':memory:' or os.path.exists(fname)

def drop_db_sqlite(uri):
    fname = uri['database']
    if fname != ':memory:':
        os.remove(fname)

def create_db_sqlite(uri):
    "Do nothing, since the db is automatically created"
