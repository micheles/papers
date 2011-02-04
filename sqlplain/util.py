"""
Notice: create_db and drop_db are not transactional.
"""

import os, sys, re, subprocess, tempfile
from sqlplain.uri import URI, CODEMAP
from sqlplain.sql_support import get_args_templ
from sqlplain import connect, do
from sqlplain.connection import Transaction
from sqlplain.namedtuple import namedtuple
try:
    CalledProcessError = subprocess.CalledProcessError
except AttributeError: # Python < 2.5
    class CalledProcessError(Exception):
        def __init__(self, returncode, cmd):
            self.returncode = returncode
            self.cmd =cmd

chatty = False    

def getoutput(commandlist, stdin=None, save_on=None):
    '''
    Returns the output of a system command or raise a CalledProcessError.
    Optionally, saves the output on save_on (a writable file-like object).
    '''
    if stdin is None:
        stdin = subprocess.PIPE
    if save_on is None:
        save_on = subprocess.PIPE
    elif isinstance(save_on, str): # assume it is a filename
        save_on = file(save_on, 'w')
    po = subprocess.Popen(commandlist, stdin=stdin, stdout=save_on)
    if chatty:
        print 'Running %s' % ' '.join(map(repr, commandlist))
    out, err = po.communicate()
    if po.returncode or err:
        if err:
            sys.stderr.write(err)
            sys.stderr.flush()
        cmd_str = ''
        for cmd in commandlist:
            if re.search(r'\s', cmd):
                cmd_str += repr(cmd) + " "
            else:
                cmd_str += cmd + " "
        raise CalledProcessError(po.returncode, cmd_str)
    return out
    
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
    conn = connect(uri, isolation_level)
    try:
        if isolation_level is None:
            return conn.execute(templ, args)
        else:
            return Transaction(conn.__class__.execute, conn, templ, args).run()
    finally:
        conn.close()

def exists_db(uri):
    "Check is a database exists"
    return _call('exists_db', URI(uri))

def drop_db(uri):
    "Drop an existing database"
    _call('drop_db', URI(uri))

# helper for create_db
def _collect(directory, exts):
    '''
    Read the files with a given set of extensions from a directory
    and returns them ordered by version number.
    '''
    chunks = []
    for fname in os.listdir(directory):
        if fname.endswith(exts) and not fname.startswith('_'):
            version = VERSION.search(fname)
            if version:
                code = file(os.path.join(directory, fname), 'U').read()
                chunks.append(Chunk(version, fname, code))
    return sorted(chunks) # chunks are named tuples

def runscripts(db, scriptdir, exts):
    for chunk in _collect(scriptdir, exts):
        if chunk.fname.endswith('.sql'):
            if chatty:
                print "EXECUTING %s" % chunk.fname
            db.executescript(chunk.code)
        elif chunk.fname.endswith('.py'):
            exec chunk.code in {}

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
            drop_db(uri)
        else:
            raise RuntimeError(
                'There is already a database %s!' % uri)
    _call('create_db', uri)
    db = connect(uri, **kw)
    scriptdir = uri.scriptdir or scriptdir
    if scriptdir:
        runscripts(db, scriptdir, ('.sql', '.py'))
    return db

def create_table(conn, tname, body, force=False):
    """
    Create a table. If the table already exists, raise an error, unless
    force is True.
    """
    if exists_table(conn, tname) and force:
        drop_table(conn, tname) # do not raise an error
    return conn.execute('CREATE TABLE %s(\n%s)' % (tname, body))

def drop_table(conn, tname, force=False):
    """
    Drop a table. If the table does not exist, raise an error, unless
    force is True.
    """
    if not exists_table(conn, tname) and force:
        return # do not raise an error
    return conn.execute('DROP TABLE %s' % tname)

def _make_clause(dic, sep):
    '''
    An utility function to generate an SQL clause from a dictionary:
    >>> make_clause(dict(a=1, b=2), ' AND ')
    ('a=:a AND b=:b', (1, 2))
    '''
    clauses = [] 
    vals = []
    for n, v in dic.iteritems():
        clauses.append('%s=:%s' % (n, n))
        vals.append(v)
    return sep.join(clauses), tuple(vals)

def update_table(conn, tname, vdict, **kdict):
    "A low-level utility to update a table"
    where, kvals = _make_clause(kdict, ' AND ')
    set_, vvals = _make_clause(vdict, ', ')
    templ = 'UPDATE %s SET %s WHERE %s' % (tname, set_, where)
    return conn.execute(templ, vvals + kvals)

def copy_table(conn, src, dest, force=False):
    """
    Copy src into dest by using SELECT INTO; dest must be a valid tablename.
    If force is True and dest is an already existing table, dest is
    destroyed and recreated, and a primary key is added.
    """
    query = "SELECT * INTO %s FROM %s" % (dest, src)
    recreate = force and exists_table(conn, dest)
    if recreate:
        drop_table(conn, dest)
    n = conn.execute(query)
    if recreate:
        kfields = ', '.join(get_kfields(conn, src))
        conn.execute('ALTER TABLE %s ADD PRIMARY KEY (%s)' % (dest, kfields))
    return n

def remote_copy_table(remote_db, local_db, src, dest, mode='b', truncate=False):
    """
    Return the temporary file used.
    """
    fd, tempname = tempfile.mkstemp()
    try:
        dump_file(remote_db.uri, src, tempname, mode)
        if truncate:
            truncate_table(local_db, dest)
        print load_file(local_db.uri, dest, tempname, mode)
    finally:
        return tempname

def truncate_table(conn, tname):
    if conn.dbtype == 'sqlite': 
        # TRUNCATE is not supported right now
        return conn.execute('DELETE FROM %s' % tname)
    else:
        return conn.execute('TRUNCATE TABLE %s' % tname)

def insert_rows(conn, tname, rows):
    """Insert an iterable sequence of rows into a table; 
    useful for unit tests. Notice that it invokes executemany
    on the underlying low-level connection"""
    lst = list(rows)
    n = 0 # number of inserted lines
    try:
        row = lst[0]
    except IndexError: # nothing to insert
        return n
    numeric = [':%s' % (i + 1) for i in range(len(row))]
    templ = 'INSERT INTO %s VALUES (%s)' % (tname, ', '.join(numeric))
    argnames, raw_templ = get_args_templ(templ)
    return conn._storage.curs.executemany(raw_templ, rows)
    
def load_file(uri, tname, fname, mode, **kwargs):
    "Bulk insert a (binary or csv) file into a table"""
    assert mode in 'bc', 'Mode must be "b" (binary) or "c" (csv)'
    return _call('load_file', uri, tname, fname, mode, **kwargs)

def dump_file(uri, query, fname, mode, **kwargs):
    "Dump the result of a query into a (binary or csv) file"
    assert mode in 'bc', 'Mode must be "b" (binary) or "c" (csv)'
    return _call('dump_file', uri, query, fname, mode, **kwargs)
    
########################## introspection routines ######################

def get_sizeK(conn, table):
    return _call('get_sizeK', conn, table)

def get_tables(conn, schema=None):
    """Return the names of the tables in the current database 
    (and schema, if any)"""
    return _call('get_tables', conn, schema)

def get_views(conn, schema=None):
    """Return the names of the views in the current database 
    (and schema, if any)"""
    return _call('get_views', conn, schema)

def exists_table(conn, tname, schema=None):
    "Check if a table exists"
    return _call('exists_table', conn, tname)

def get_descr(conn, tname):
    "Return the DB API 2 description as a list of rows"
    return conn.execute('SELECT * FROM %s WHERE 1=0;' % tname).descr

def inspect_columns(conn, tname, tuples=False):
    """
    Return a list of strings "fieldname fieldtype(size)" or of tuples
    (fieldname, fieldtype, size).
    """
    codemap = CODEMAP[conn.dbtype]
    ls = []
    for x in get_descr(conn, tname):
        fname, ftype, fsize = x.name, codemap[x.type_code], x.internal_size 
        if tuples:
            ls.append((fname, ftype, fsize))
        else:
            ls.append('%s %s%s' %
                      (fname, ftype, '(%s)' % fsize
                       if ftype=='VARCHAR' and fsize>0 else ''))
    return ls

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

set_schema = do('SET search_path TO :schema')

exists_schema = do("SELECT nspname FROM pg_namespace WHERE nspname=:schema")

def drop_schema(db, schema):
    db.execute('DROP SCHEMA %s CASCADE' % schema)

def create_schema(db, schema, force=False, schema_dir=None):
    """
    Create the specified schema. If the schema exists already
    an error is raised, unless force is True: in that case the schema
    is dropped and recreated. We are left in the created schema.
    """
    if force and exists_schema(db, schema):        
        drop_schema(db, schema)
    db.execute('CREATE SCHEMA %s' % schema)
    set_schema(db, schema)
    if schema_dir:
        runscripts(db, schema_dir, ('.sql', '.py'))
