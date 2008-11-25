import os, sys, subprocess, re
from sqlplain.configurator import configurator
from sqlplain.connection import Connection
from sqlplain.util import createdb, createschema
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

def makedb(alias=None, uri=None, dir=None):
    if alias is not None and uri is None:
        uri = configurator.uri[alias]
    if alias is not None and dir is None:
        dir = configurator.dir[alias]
    db = createdb(uri, drop=True)
    chunks = collect(dir, ('.sql', '.py'))
    for chunk in chunks:
        if chunk.fname.endswith('.sql'):
            db.execute(chunk.code)
        elif chunk.fname.endswith('.py'):
            exec chunk.code in {}
        
def makeschema(alias=None, schema=None, uri=None, dir=None):
    if alias is not None and uri is None:
        uri = configurator.uri[alias]
    if alias is not None and dir is None:
        dir = configurator.dir[alias]
    db = Connection(uri)
    schema = createschema(db, schema, drop=True)
    chunks = collect(dir, ('.sql', '.py'))
    for chunk in chunks:
        if chunk.fname.endswith('.sql'):
            db.execute(chunk.code)
        elif chunk.fname.endswith('.py'):
            exec chunk.code in {}
        
if __name__ == '__main__':
    makedb('utest')
    makedb('autest')
    makedb('ftest')
