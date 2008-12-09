"""
Notice: create_db and drop_db are not transactional.
"""

import os, sys, re
from sqlplain.uri import URI
from sqlplain import lazyconnect, transact, do
from sqlplain.namedtuple import namedtuple

VERSION = re.compile(r'(\d[\d\.-]+)')
Chunk = namedtuple('Chunk', 'version fname code')

def _call(procname, uri_or_conn, *args, **kw):
    "Call a procedure by name, by dispatching on the database type"
    dbtype = uri_or_conn.dbtype
    proc = globals().get(procname + '_' + dbtype)
    if proc is None:
        raise NameError('Missing procedure %s for %s' % (procname, dbtype))
    return proc(uri_or_conn, *args, **kw)

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
    return _call('exists_db', URI(uri))

def drop_db(uri):
    "Drop an existing database"
    _call('drop_db', URI(uri))

# helper for createdb
def _collect(directory, exts):
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

def create_db(uri, force=False, scriptdir=None, **kw):
    """
    Create the database specified by uri. If the database exists already
    an error is raised, unless force is True: in that case the database
    is dropped and recreated.
    """
    uri = URI(uri)
    uri.import_driver() # import the driver
    if exists_db(uri):
        if force:
            _call('drop_db', uri)
        else:
            raise RuntimeError(
                'There is already a database %s!' % uri)
    _call('create_db', uri)
    db = lazyconnect(uri, **kw)
    scriptdir = uri.scriptdir or scriptdir
    if scriptdir:
        chunks = _collect(scriptdir, ('.sql', '.py'))
        for chunk in chunks:
            if chunk.fname.endswith('.sql'):
                db.executescript(chunk.code)
            elif chunk.fname.endswith('.py'):
                exec chunk.code in {}
    return db

def bulk_insert(conn, file, table, sep='\t'):
    return _call('bulk_insert', conn, file, table, sep)

def exists_table(conn, tname):
    "Check if a table exists"
    return _call('exists_table', conn, tname)

def drop_table(conn, tname, force=False):
    """
    Drop a table. If the table does not exist, raise an error, unless
    force is True.
    """
    if not exists_table(tname) and force:
        return # do not raise an error
    return conn.execute('DROP TABLE %s' % tname)

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
