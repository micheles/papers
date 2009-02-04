import sys
from sqlplain.util import openclose, getoutput

BCP = ['freebcp', 'bcp'][sys.platform == 'win32']

def get_kfields_mssql(conn, table):
    return [x.COLUMN_NAME for x in conn.execute('sp_pkeys %s' % table)]

def dump_file_mssql(conn, fname, table_or_query, sep='\t', null='\N'):
    """
    Examples:
    >> dump_file(conn, 'client.csv', 'client')
    >> dump_file(conn, 'client.csv',
      'select * from %(database)s..client' % conn.uri) 
    """
    uri = conn.uri
    if table_or_query.lstrip().lower().startswith('select'):
        out = 'queryout'
    elif not '.' in table_or_query:
        out = 'out'
        table_or_query = '.'.join([uri['database'], '', table_or_query])
    cmd = [BCP, table_or_query, out, fname, '-S', uri['host'],
           '-U', uri['user'], '-P', uri['password'], '-c',  '-t', sep]
    return getoutput(cmd)

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

def bcp_dump(uri, tablename, filename):
    "Save a table into a bcp binary file"
    cmd = [BCP, '%s..%s' % (uri['database'], tablename), 'out', filename,
           '-S', uri['host'], '-U', uri['user'], '-P', uri['password'], '-n']
    return getoutput(cmd)

def bcp_restore(uri, filename, tablename):
    "Copy from a bcp binary file into a table"
    cmd = [BCP, '%s..%s' % (uri['database'], tablename), 'in', filename,
           '-S', uri['host'], '-U', uri['user'], '-P', uri['password'], '-n']
    return getoutput(cmd)
    
