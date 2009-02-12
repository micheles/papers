import sys
from sqlplain.util import openclose, getoutput

BCP = ['freebcp', 'bcp'][sys.platform == 'win32']

def bcp(uri, table_or_query, filename, in_out, *flags):
    "Helper for calling the bcp command line utility as an external process"
    assert in_out in ('in', 'out', 'queryout')
    if table_or_query.lstrip().lower().startswith('select'):
        in_out = 'queryout'
    elif not '.' in table_or_query:
        table_or_query = '.'.join([uri['database'], '', table_or_query])
    cmd = [BCP, table_or_query, in_out, filename, '-S', uri['host'],
           '-U', uri['user'], '-P', uri['password']] + list(flags)
    return getoutput(cmd)

def get_kfields_mssql(conn, table):
    return [x.COLUMN_NAME for x in conn.execute('sp_pkeys %s' % table)]

def dump_file_mssql(uri, table_or_query, fname, mode, sep='\t', null='\N'):
    """
    Dump a table or query into a CSV file by using bcp. Examples:
    >> dump_file(conn, 'client.csv', 'client', sep='\t')
    >> dump_file(conn, 'client.csv',
      'select * from %(database)s..client' % conn.uri, sep='\t') 
    """
    if mode == 'c': # csv mode       
        return bcp(uri, table_or_query, fname, 'out', '-c', '-t', sep)
    else: # binary mode
        return bcp(uri, table_or_query, fname, 'out', '-n') 

def load_file_mssql(uri, table, fname, mode, sep='\t'):
    "Insert a file into a table via bcp"
    if mode == 'c': # csv mode
        return bcp(uri, table, fname, 'in', '-c', '-t', sep)
    else: # binary mode
        return bcp(uri, table, fname, 'in', '-n')

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

