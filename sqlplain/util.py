"""
Notice: create_db and drop_db are not transactional.
"""

import os, sys, re
from sqlplain.uri import URI
from sqlplain import lazyconnect, transact, do
from sqlplain.namedtuple import namedtuple
from sqlplain.connection import connmethod

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
    
############################### Inserter ###############################

def insert(ttuple):
    "Return a procedure inserting a row or a dictionary into a table"
    name = ttuple.__name__
    csfields = ','.join(ttuple._fields)
    qmarks = ','.join('?'*len(ttuple._fields))
    templ = 'INSERT INTO %s (%s) VALUES (%s)' % (name, csfields, qmarks)
    def insert_row(conn, row=None, **kw):
        row = row or {}
        if isinstance(row, dict):
            row.update(kw)
            row = ttuple(**row)
        return conn.execute(templ, row)
    insert_row.__doc__ = insert_row.templ = templ
    return insert_row

class Inserter(object):
    @classmethod
    def type(cls, name, fields):
        "Ex. Book = Table.type('book', 'serial', 'title author')"
        tt = namedtuple(name, fields)
        ins = connmethod(insert(tt))
        return type(name.capitalize(), (cls,), dict(tt=tt, insert_row=ins))

    @classmethod
    def object(cls, conn, name):
        "Ex. book = Table.object(mydb, 'book')"
        fields = util.get_fields(conn, name)
        return cls.type(name, fields)(conn)

    @connmethod
    def insert_from(conn, file, table, sep='\t'):
        'Populate a table by reading a file-like object'
        return _call('bulk_insert', conn, file, table, sep)

    def __init__(self, conn):
        self.tt # raise AttributeError if not initialized correctly
        self.conn = conn
    
########################## introspection routines ######################

def exists_table(conn, tname):
    "Check if a table exists"
    return _call('exists_table', conn, tname)

def get_descr(conn, name):
    return conn.execute('SELECT * FROM %s WHERE 1=0;' % name).descr
    
def get_fields(conn, tname):
    return [x.name for x in get_descr(conn, tname)]

def get_kfields(conn, tname):
    return  _call('get_kfields', conn, tname)

def get_dfields(conn, name):
    kfields = set(get_kfields(conn, name))
    return [f.name for f in get_descr(conn, name) if f.name not in kfields]

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
