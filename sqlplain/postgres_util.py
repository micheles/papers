from sqlplain.util import openclose
from sqlplain.automatize import getoutput

def create_db_postgres(uri):
    openclose(uri.copy(database='template1'),
              'CREATE DATABASE %(database)s' % uri)

def drop_db_postgres(uri):
    openclose(uri.copy(database='template1'),
              'DROP DATABASE %(database)s' % uri)

def exists_table_postgres(conn, tname):
    return conn.execute('SELECT count(*) FROM pg_tables WHERE name=?',
                        tname, scalar=True)

def bulk_insert_postgres(conn, file, table, sep='\t', null='\N', columns=None):
    conn._curs.copy_from(file, table, sep, null, columns)

def dump_postgres(conn, file, table, sep='\t', null='\N', columns=None):
    conn._curs.copy_to(file, table, sep, null, columns)

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
