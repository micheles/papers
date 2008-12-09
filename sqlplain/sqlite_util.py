import os
from sqlplain.util import openclose

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
