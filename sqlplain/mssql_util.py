import re, sys
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

def _normalize(ref):
    """Convert a string of the form REFERENCES dbname.dbowner.tname into
    REFERENCES tname."""
    references, rest = ref.split(' ', 1)
    return references + ' ' + rest.split('.')[-1]

## similar info also comes from get_descr
def get_columns_mssql(conn, table):
    return conn.execute('sp_columns %s' % table)

def get_keys_mssql(conn, table):
    """
    Return a list of strings describing the keys (both primary and foreign)
    of a given table.
    """
    info = iter(conn.execute("sp_helpconstraint %s, 'nomsg'" % table))
    result = []
    for row in info:
        ctype = row.constraint_type
        if ctype == 'FOREIGN KEY':
            nextrow = info.next()
            ref = '(%s) %s' % (row.constraint_keys, _normalize(
                    nextrow.constraint_keys))
        elif ctype.startswith('PRIMARY KEY'):
            ref = '(%s)' % row.constraint_keys
        else: # other column type
            continue
        result.append('%s %s' % (ctype, ref))
    return result

def get_dependencies(conn, *tables):
    s = set()
    for table in tables:
        for line in get_keys_mssql(conn, table):
            if line.startswith('FOREIGN KEY'):
                mo = re.search('REFERENCES ([\w\d]+) ', line)
                s.add((table.lower(), mo.group(1).lower()))
    return sorted(s)

# works only for views and procedures, and constraints
GET_DEF = '''SELECT definition FROM sys.sql_modules WHERE object_id=\
(SELECT object_id FROM sys.objects WHERE name=:name)'''

def get_source_mssql(conn, objname):
    "Extracts the source code for views and procedures"
    return conn.execute(GET_DEF, (objname,), scalar=True)

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

## introspection

# TODO: add schema
def get_sizeK_mssql(conn, table):
    size = conn.execute('sp_spaceused %s' % table)[0] # first row
    if size.data:
        return int(size.data.split()[0])
    else:
        return 0

def get_tables_mssql(conn, schema=None):
    if schema:
        return [r.TABLE_NAME for r in conn.execute('sp_tables') 
                if r.TABLE_OWNER==schema and r.TABLE_TYPE == 'TABLE']
    else:
        return [r.TABLE_NAME for r in conn.execute('sp_tables')
                if r.TABLE_TYPE == 'TABLE']
 
def exists_table_mssql(conn, tname):
    return conn.execute('SELECT count(*) FROM sysobjects WHERE name=:name',
                        (tname,), scalar=True)

def exists_db_mssql(uri):
    dbname = uri['database']
    master = uri.copy(database='master')
    # for misterious reasons you must be transactional to use sp_databases
    for row in openclose(
        master, 'sp_databases', isolation_level='SERIALIZABLE'):
        if row[0] == dbname:
            return True
    return False
    
def drop_db_mssql(uri):
    openclose(uri.copy(database='master'),
              'DROP DATABASE %(database)s' % uri)
  
def create_db_mssql(uri):
    openclose(uri.copy(database='master'),
              'CREATE DATABASE %(database)s' % uri)

