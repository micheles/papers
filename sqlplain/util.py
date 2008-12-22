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

def drop_table(conn, tname, force=False):
    """
    Drop a table. If the table does not exist, raise an error, unless
    force is True.
    """
    if not exists_table(conn, tname) and force:
        return # do not raise an error
    return conn.execute('DROP TABLE %s' % tname)

def copy_table(conn, src, dest, force=False):
    """
    Copy src into dest by using SELECT INTO; dest must be a valid tablename.
    If force is True and dest is an already existing table, dest is
    destroyed and recreated.
    """
    query = "SELECT * INTO %s FROM %s" % (dest, src)
    if force and exists_table(conn, dest):
        drop_table(conn, dest)
    n = conn.execute(query)
    kfields = ', '.join(get_kfields(conn, src))
    conn.execute('ALTER TABLE %s ADD PRIMARY KEY (%s)' % (dest, kfields))
    return n

def truncate_table(conn, tname):
    if conn.dbtype == 'sqlite': # TRUNCATE is not supported right now
        #conn.execute('PRAGMA synchronous = OFF')
        try:
            return conn.execute('DELETE FROM %s' % tname)
        finally:
            pass
            #conn.execute('PRAGMA synchronous = ON')
    else:
        return conn.execute('TRUNCATE TABLE %s' % tname)

def insert_rows(conn, tname, rows):
    "Insert an iterable sequence of rows into a table; useful for unit tests"
    it = iter(rows)
    n = 0 # number of inserted lines
    try:
        row = it.next()
    except StopIteration: # nothing to insert
        return n
    templ = 'INSERT INTO %s VALUES (%s)' % (tname, ', '.join('?'*len(row)))
    n = conn.execute(templ, row)
    for row in it:
        n += conn.execute(templ, row)
    return n
    
def insert_file(conn, fname, tname, sep=','):
    "Bulk insert a CSV file into a table"""
    return _call('insert_file', conn, fname, tname, sep)

########################## introspection routines ######################

def exists_table(conn, tname):
    "Check if a table exists"
    return _call('exists_table', conn, tname)

def get_descr(conn, tname):
    "Return the DB API 2 description as a list of rows"
    return conn.execute('SELECT * FROM %s WHERE 1=0;' % tname).descr
    
def get_fields(conn, tname):
    """
    Return the names of the columns of a table (must be ASCII).
    """
    return [x.name for x in get_descr(conn, tname)]

def get_kfields(conn, tname):
    """
    Return the names of the primary key column(s) of a table (must be ASCII).
    """
    return map(str, _call('get_kfields', conn, tname))

def get_dfields(conn, tname):
    """
    Return the names of the data column(s) of a table (must be ASCII).
    """
    kfields = set(get_kfields(conn, tname))
    return [name for name in get_fields(conn, tname) if name not in kfields]

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
