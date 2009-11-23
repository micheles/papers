import os
from sqlplain.util import openclose, insert_rows
from sqlplain import connect

def get_info(conn, tname):
    """
    Returns a list of namedtuples [(cid, name, type, notnull, dflt_value, pk)]
    """
    return conn.execute('PRAGMA table_info(%s)' % tname)

def load_file_sqlite(uri, tname, fname, mode, sep):
    import csv
    assert mode == 'c', "Only CSV files can be bulk imported in sqlite"
    csvfile = file(fname)
    conn = connect(uri)
    conn.execute('PRAGMA synchronous = OFF')
    try:
        n = insert_rows(conn, tname, csv.reader(csvfile, delimiter=sep))
    finally:
        csvfile.close()
        conn.execute('PRAGMA synchronous = ON')
    return n

def get_kfields_sqlite(conn, tname):
    return [x.name for x in get_info(conn, tname) if x.pk]

def get_tables_sqlite(conn):
    return [r.name for r in conn.execute('PRAGMA table_info')]

def exists_table_sqlite(conn, tname):
    res = conn.execute('PRAGMA table_info(%s)' % tname)
    return res != -1

def exists_db_sqlite(uri):
    fname = uri['database']
    return fname == ':memory:' or os.path.exists(fname)

def drop_db_sqlite(uri):
    fname = uri['database']
    if fname != ':memory:':
        os.remove(fname)

def create_db_sqlite(uri):
    "Do nothing, since the db is automatically created"
