"""

Notice: create_db and drop_db are not transactional.
"""

import os
from sqlplain.uri import URI
from sqlplain import lazyconnect, transact, do
from sqlplain.namedtuple import namedtuple

def insert(ntuple):
    name = ntuple.__name__
    csfields = ','.join(ntuple._fields)
    qmarks = ','.join('?'*len(ntuple._fields))
    templ = 'INSERT INTO %s (%s) VALUES (%s)' % (name, csfields, qmarks)
    def _insert(conn, row):
        if isinstance(row, dict):
            row = ntuple(**row)        
        return conn.execute(templ, row)
    _insert.__name__ = templ
    return _insert

def openclose(uri, templ, *args, **kw):
    "Open a connection, perform an action and close the connection"
    unexpected = set(kw) - set(['isolation_level'])
    if unexpected:
        raise ValueError('Received unexpected keywords: %s' % unexpected)
    isolation_level = kw.get('isolation_level', None)
    conn = lazyconnect(uri, isolation_level)
    try:
        if isolation_level is None:
            return conn.execute(templ, args)
        else:
            return transact(conn.__class__.execute, conn, templ, args)
    finally:
        conn.close()

def call(procname, uri):
    "Call a procedure by name, passing to it an URI string"
    proc = globals()[procname + '_' + uri['dbtype']]
    return proc(uri)

################################ exists_db ###############################

def exists_db_sqlite(uri):
    fname = uri['database']
    return fname == ':memory:' or os.path.exists(fname)

def exists_db_postgres(uri):
    dbname = uri['database']
    for row in openclose(
        uri.copy(database='template1'), 'SELECT datname FROM pg_database'):
        if row[0] == dbname:
            return True
    return False

def exists_db_mssql(uri):
    dbname = uri['database']
    master = uri.copy(database='master')
    for row in openclose(master, 'sp_databases', autocommit=False):
        if row[0] == dbname:
            return True
    return False
    
def exists_db(uri):
    return call('exists_db', URI(uri))

############################### drop_db ###################################

def drop_db_sqlite(uri):
    fname = uri['database']
    if fname != ':memory:':
        os.remove(fname)
    
def drop_db_postgres(uri):
    openclose(uri.copy(database='template1'),
              'DROP DATABASE %(database)s' % uri)

def drop_db_mssql(uri):
    openclose(uri.copy(database='master'),
              'DROP DATABASE %(database)s' % uri)
  
def drop_db(uri):
    call('drop_db', URI(uri))
    
############################# create_db ###################################

def create_db_sqlite(uri):
    "Do nothing, since the db is automatically created"

def create_db_postgres(uri):
    openclose(uri.copy(database='template1'),
              'CREATE DATABASE %(database)s' % uri)

def create_db_mssql(uri):
    openclose(uri.copy(database='master'),
              'CREATE DATABASE %(database)s' % uri)

def create_db(uri, force=False, **kw):
    uri = URI(uri)
    if exists_db(uri):
        if force:
            call('drop_db', uri)
        else:
            raise RuntimeError(
                'There is already a database %s!' % uri)
    call('create_db', uri)
    return lazyconnect(uri, **kw)

########################## schema management ###########################

## the folling routines are postgres-only

set_schema = do('SET search_path TO ?')

exists_schema = do("SELECT nspname FROM pg_namespace WHERE nspname=?")

def drop_schema(db, schema):
    db.execute('DROP SCHEMA %s CASCADE' % schema)

def create_schema(db, schema, force=False):
    if force and exists_schema(db, schema):        
        drop_schema(db, schema)
    db.execute('CREATE SCHEMA %s' % schema)
    set_schema(db, schema)
