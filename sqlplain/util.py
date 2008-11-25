"""

Notice: createdb and dropdb are not transactional.
"""

import os
from sqlplain.uri import URI
from sqlplain.connection import Connection, transact, do

def openclose(uri, templ, *args, **kw):
    "Open a connection, perform an action and close the connection"
    unexpected = set(kw) - set(['autocommit'])
    if unexpected:
        raise ValueError('Received unexpected keywords: %s' % unexpected)
    autocommit = kw.get('autocommit', True)
    conn = Connection(uri, autocommit)
    try:
        if autocommit:
            return conn.execute(templ, args)
        else:
            return transact(Connection.execute, conn, templ, args)
    finally:
        conn.close()

def call(procname, uri):
    "Call a procedure by name, passing to it an URI string"
    proc = globals()[procname + '_' + uri['dbtype']]
    return proc(uri)

################################ exists_db ###############################

def existsdb_sqlite(uri):
    fname = uri['database']
    return fname == ':memory:' or os.path.exists(fname)

def existsdb_postgres(uri):
    dbname = uri['database']
    for row in openclose(
        uri.copy(database='template1'), 'SELECT datname FROM pg_database'):
        if row[0] == dbname:
            return True
    return False

def existsdb_mssql(uri):
    dbname = uri['database']
    master = uri.copy(database='master')
    for row in openclose(master, 'sp_databases', autocommit=False):
        if row[0] == dbname:
            return True
    return False
    
def existsdb(uri):
    return call('existsdb', URI(uri))

############################### dropdb ###################################

def dropdb_sqlite(uri):
    fname = uri['database']
    if fname != ':memory:':
        os.remove(fname)
    
def dropdb_postgres(uri):
    openclose(uri.copy(database='template1'),
              'DROP DATABASE %(database)s' % uri)

def dropdb_mssql(uri):
    openclose(uri.copy(database='master'),
              'DROP DATABASE %(database)s' % uri)
  
def dropdb(uri):
    call('drop_db', URI(uri))
    
############################# createdb ###################################

def createdb_sqlite(uri):
    "Do nothing, since the db is automatically created"

def createdb_postgres(uri):
    openclose(uri.copy(database='template1'),
              'CREATE DATABASE %(database)s' % uri)

def createdb_mssql(uri):
    openclose(uri.copy(database='master'),
              'CREATE DATABASE %(database)s' % uri)

def createdb(uri, drop=False):
    uri = URI(uri)
    if drop and existsdb(uri):        
        call('dropdb', uri)
    call('createdb', uri)
    return Connection(uri)

########################## schema management ###########################

## the folling routines are postgres-only

setschema = do('SET search_path TO ?')

existsschema = do("SELECT nspname FROM pg_namespace WHERE nspname=?")

def dropschema(db, schema):
    db.execute('DROP SCHEMA %s' % schema)

def createschema(db, schema, drop=False):
    if drop and existsschema(db, schema):        
        dropschema(db, schema)
    db.execute('CREATE SCHEMA %s' % schema)
    setschema(db, schema)
