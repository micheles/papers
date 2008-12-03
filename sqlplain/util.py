"""
Notice: create_db and drop_db are not transactional.
"""

import os, sys
from sqlplain.uri import URI
from sqlplain import lazyconnect, transact, do
from sqlplain.namedtuple import namedtuple

VERSION = re.compile(r'(\d[\d\.-]+)')
Chunk = namedtuple('Chunk', 'version fname code')

def collect(directory, exts):
    '''
    Read the files with a given set of extensions from a directory
    and returns them ordered by version number.
    '''
    sql = []
    for fname in os.listdir(directory):
        if fname.endswith(exts) and not fname.startswith('_'):
            version = VERSION.search(fname)
            if version:
                code = file(os.path.join(directory, fname)).read()
                sql.append(Chunk(version, fname, code))
    return sorted(sql)

# dispatch on the database type

def _call_with_uri(procname, uri, *args):
    "Call a procedure by name, passing to it an URI string"
    proc = globals().get(procname + '_' + uri['dbtype'])
    if proc is None:
       raise NameError('Missing procedure %s, database not supported' %
                       proc.__name__) 
    return proc(uri, *args)

def _call_with_conn(procname, conn, *args):
    proc = globals().get(procname + '_' + conn.dbtype)
    if proc is None:
       raise NameError('Missing procedure %s, database not supported' %
                       proc.__name__) 
    return proc(conn, *args)

# exported utilities

def openclose(uri, templ, *args, **kw):
    "Open a connection, perform an action and close the connection"
    unexpected = set(kw) - set(['isolation_level'])
    if unexpected:
        raise ValueError('Received unexpected keywords: %s' % unexpected)
    isolation_level = kw.get('isolation_level', None)
    conn = lazyconnect(uri, isolation_level)
    try:
        if isolation_level is None:
            return conn.execute(templ, args)
        else:
            return transact(conn.__class__.execute, conn, templ, args)
    finally:
        conn.close()

def exists_db(uri):
    "Check is a database exists"
    return _call_with_uri('exists_db', URI(uri))

def drop_db(uri):
    "Drop an existing database"
    _call_with_uri('drop_db', URI(uri))

def create_db(uri, force=False, scriptdir=None, **kw):
    """
    Create the database specified by uri. If the database exists already
    an error is raised, unless force is True: in that case the database
    is dropped and recreated.
    """
    uri = URI(uri)
    if exists_db(uri):
        if force:
            _call_with_uri('drop_db', uri)
        else:
            raise RuntimeError(
                'There is already a database %s!' % uri)
    _call_with_uri('create_db', uri)
    db = lazyconnect(uri, **kw)
    if scriptdir:
        chunks = collect(dir, ('.sql', '.py'))
        for chunk in chunks:
            if chunk.fname.endswith('.sql'):
                db.execute(chunk.code)
            elif chunk.fname.endswith('.py'):
                exec chunk.code in {}
    return db

def bulk_insert(conn, file, table, sep='\t'):
    return _call_with_conn('bulk_insert', conn, file, table, sep)

def exists_table(conn, tname):
    "Check if a table exists"
    return _call_with_conn(conn, tname)

########################## schema management ###########################

## the folling routines are postgres-only

set_schema = do('SET search_path TO ?')

exists_schema = do("SELECT nspname FROM pg_namespace WHERE nspname=?")

def drop_schema(db, schema):
    db.execute('DROP SCHEMA %s CASCADE' % schema)

def create_schema(db, schema, force=False):
    """
    Create the specified schema. If the schema exists already
    an error is raised, unless force is True: in that case the schema
    is dropped and recreated.
    """
    if force and exists_schema(db, schema):        
        drop_schema(db, schema)
    db.execute('CREATE SCHEMA %s' % schema)
    set_schema(db, schema)
