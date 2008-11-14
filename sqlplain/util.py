"""

Notice: createdb and dropdb are not transactional.
"""
__all__ = 'existsdb dropdb createdb'.split()

import os
from sqlplain.uri import URI
from sqlplain.connection import Connection, openclose

# helper
def call(procname, uri):
    proc = globals()[procname + '_' + uri['dbtype']]
    return proc(uri)

############################# exists_db ############################

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

############################# dropdb ##################################

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
    
############################# createdb #####################################3

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
