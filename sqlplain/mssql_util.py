from sqlplain.util import openclose

def get_kfields_mssql(conn, table):
    return [x.COLUMN_NAME for x in conn.execute('sp_pkeys %s' % table)]

def insert_file_mssql(uri, fname, table, sep='\t'):
    return conn.execute(
        'BULK INSERT %s FROM ? SEPARATOR ?' % table, (fname, sep))
    
def get_tables(conn):
    return [r.name for r in conn.execute('SELECT name FROM sysobjects')]
    
def exists_table_mssql(conn, tname):
    return conn.execute('SELECT count(*) FROM sysobjects WHERE name=?',
                        (tname,), scalar=True)

def exists_db_mssql(uri):
    dbname = uri['database']
    master = uri.copy(database='master')
    # for misterious reasons you must be transactional to use sp_databases
    for row in openclose(master, 'sp_databases', autocommit=False):
        if row[0] == dbname:
            return True
    return False
    
def drop_db_mssql(uri):
    openclose(uri.copy(database='master'),
              'DROP DATABASE %(database)s' % uri)
  
def create_db_mssql(uri):
    openclose(uri.copy(database='master'),
              'CREATE DATABASE %(database)s' % uri)
