from sqlplain.util import openclose, getoutput

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

def get_tables_postgres(conn):
    return [r[0] for r in conn.execute('SELECT tablename FROM pg_tables')]

def exists_table_postgres(conn, tname):
    return conn.execute('SELECT count(*) FROM pg_tables WHERE tablename=?',
                        (tname,), scalar=True)

# apparently copy_from from psycopg2 is buggy for large files
def insert_file_postgres(conn, fname, table, sep=',', null='\N'):
    templ = "COPY %s FROM ? WITH DELIMITER ? NULL ?"
    return conn.execute(templ % table, (fname, sep, null))

def dump_file_postgres(conn, fname, query, sep='\t', null='\N'):
    sql = "COPY %s TO ? WITH DELIMITER ? NULL ?" % query
    return conn.execute(sql, (fname, sep, null))

def exists_db_postgres(uri):
    dbname = uri['database']
    for row in openclose(
        uri.copy(database='template1'), 'SELECT datname FROM pg_database'):
        if row[0] == dbname:
            return True
    return False

def pg_dump(uri, *args):
    cmd = ['pg_dump', '-h', uri['host'], '-U', uri['user'],
           '-d', uri['database']] + list(args)
    return getoutput(cmd)

def pg_restore(uri, *args):
    cmd = ['pg_restore', '-h', uri['host'], '-U', uri['user'],
           '-d', uri['database']] + list(args)
    return getoutput(cmd)
