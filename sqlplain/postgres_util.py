import os
from sqlplain.util import openclose, getoutput

GET_PKEYS = '''\
SELECT attname FROM pg_attribute
WHERE attrelid = (
   SELECT indexrelid FROM pg_index AS i
   WHERE i.indrelid = (SELECT oid FROM pg_class WHERE relname=:table)
   AND i.indisprimary = 't')
ORDER BY attnum
'''

def get_kfields_postgres(conn, tname):
    return [x.attname for x in conn.execute(GET_PKEYS, (tname,))]

def create_db_postgres(uri):
    openclose(uri.copy(database='template1'),
              'CREATE DATABASE %(database)s' % uri)

def drop_db_postgres(uri):
    openclose(uri.copy(database='template1'),
              'DROP DATABASE %(database)s' % uri)

def get_sizeK_postgres(conn, table):
    return conn.execute('SELECT relpages*8 FROM pg_class WHERE relname=:table', 
                        (table,), scalar=True)

def get_tables_postgres(conn, schema):
    query = 'SELECT tablename FROM pg_tables'
    if schema:
        res = conn.execute(query + ' WHERE schemaname=:a', (schema,))
    else:
        res = conn.execute(query)
    return [r[0] for r in res]

def exists_table_postgres(conn, tname):
    return conn.execute('SELECT count(*) FROM pg_tables WHERE tablename=:table',
                        (tname,), scalar=True)
    
def exists_db_postgres(uri):
    dbname = uri['database']
    for row in openclose(
        uri.copy(database='template1'), 'SELECT datname FROM pg_database'):
        if row[0] == dbname:
            return True
    return False

def dump_file_postgres(uri, query, filename, mode, sep='\t', null='\N',
                       schema=None):
    """
    Save the result of a query on a local file by using COPY TO and psql
    """
    if not ' ' in query: # assumes a table name was given
        query = '(select * from %s)' % query
    else:
        query = '(%s)' % query
    if mode == 'b':
        return psql(uri, "COPY %s TO STDOUT BINARY" % query, filename, schema)
    else:
        return psql(
            uri, "COPY %s TO STDOUT WITH DELIMITER '%s' NULL '%s'" %
            (query, sep, null), filename, schema)

# apparently copy_from from psycopg2 is buggy for large files
def load_file_postgres(uri, tname, filename, mode, sep='\t', null='\N',
                       schema=None):
    """
    Load a file into a table by using COPY FROM and psql
    """
    stdin = file(filename)
    if mode == 'b':
        return psql(uri, "COPY %s FROM STDIN BINARY" % tname, stdin=stdin,
                    schema=schema)
    else: # csv mode
        copy_from = "COPY %s FROM STDIN WITH DELIMITER E'%s' NULL E'%s'" % (
            tname, sep, null)
        return psql(uri, copy_from, stdin=stdin, schema=schema)

###############################################################################

## pg_dump and pg_restore should be used for multiple tables or whole databases

def pg_dump(uri, filename, *args):
    """
    A small wrapper over pg_dump. Example:
    >> pg_dump(uri, thetable, thefile)
    """
    cmd = ['pg_dump', '-h', uri['host'], '-U', uri['user'],
           '-d', uri['database'], '-f', filename] + list(args)
    print ' '.join(cmd)
    return getoutput(cmd)

def pg_restore(uri, filename, *args):
    """
    A small wrapper over pg_restore. Example:
    >> pg_restore(uri, thetable, thefile)
    """
    cmd = ['pg_restore', '-h', uri['host'], '-U', uri['user'],
           '-d', uri['database'], filename] + list(args)
    print ' '.join(cmd)
    return getoutput(cmd)


def psql(uri, query, filename=os.devnull, stdin=None, schema=None):
    "Execute a query and save its result on filename"
    if not ' ' in query: # assumes a table name was given
        query = 'select * from %s' % query
    if schema:
        query = 'SET search_path TO %s; %s' % (schema, query) 
    cmd = ['psql', '-h', uri['host'], '-U', uri['user'], '-d', uri['database'],
           '-c', query, '-o', filename]
    # print cmd
    return getoutput(cmd, stdin)
