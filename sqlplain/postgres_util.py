from sqlplain.util import openclose
from sqlplain.automatize import getoutput

GET_PKEYS = '''\
SELECT attname FROM pg_attribute
WHERE attrelid = (
   SELECT indexrelid FROM pg_index AS i
   WHERE i.indrelid = (SELECT oid FROM pg_class WHERE relname=?)
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

def exists_table_postgres(conn, tname):
    return conn.execute('SELECT count(*) FROM pg_tables WHERE name=?',
                        tname, scalar=True)

# apparently copy_from from psycopg2 is buggy for large files
def bulk_insert_postgres(conn, fname, table, sep=',', null='\N'):
    templ = "COPY %s FROM ? WITH DELIMITER ? NULL ?"
    return conn.execute(templ % table, (fname, sep, null))

def dump_postgres(conn, fname, table, sep='\t', null='\N'):
    templ = "COPY %s TO ? WITH DELIMITER ? NULL ?"
    return conn.execute(templ % table, (fname, sep, null))

def exists_db_postgres(uri):
    dbname = uri['database']
    for row in openclose(
        uri.copy(database='template1'), 'SELECT datname FROM pg_database'):
        if row[0] == dbname:
            return True
    return False

def get_schema_postgres(uri, objectname):
    cmd = ['pg_dump', '-s',
           '-t', objectname,
           '-h', uri['host'],
           '-U', uri['user'],
           '-d', uri['database']]
    return getoutput(cmd)
