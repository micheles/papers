import os, sys, subprocess, re
from sqlplain.configurator import configurator
from sqlplain.util import create_db, create_schema
from sqlplain.namedtuple import namedtuple

VERSION = re.compile(r'(\d[\d\.-]+)')
Chunk = namedtuple('Chunk', 'version fname code')

try:
    CalledProcessError = subprocess.CalledProcessError
except AttributeError: # Python < 2.5
    class CalledProcessError(Exception): pass
    
def getoutput(commandlist):
    po = subprocess.Popen(commandlist, stdout=subprocess.PIPE)
    out, err = po.communicate()
    if po.returncode or err:
        raise CalledProcessError('%s [return code %d]' % (err, po.returncode))
    return out
    
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

def make_db(alias=None, uri=None, dir=None):
    if alias is not None and uri is None:
        uri = configurator.uri[alias]
    if alias is not None and dir is None:
        dir = configurator.dir[alias]
    db = create_db(uri, drop=True)
    chunks = collect(dir, ('.sql', '.py'))
    for chunk in chunks:
        if chunk.fname.endswith('.sql'):
            db.execute(chunk.code)
        elif chunk.fname.endswith('.py'):
            exec chunk.code in {}

def populate_db(db, dir=None):
    uri = db.uri
    if dir is None:
        alias = configurator.alias[uri]
        dir = configurator.dir[alias]
    else:
        raise TypeError('Please provide a scriptdir')
    chunks = collect(dir, ('.sql', '.py'))
    for chunk in chunks:
        if chunk.fname.endswith('.sql'):
            db.execute(chunk.code)
        elif chunk.fname.endswith('.py'):
            exec chunk.code in {}
        
def make_schema(alias=None, schema=None, uri=None, dir=None):
    if alias is not None and uri is None:
        uri = configurator.uri[alias]
    if alias is not None and dir is None:
        dir = configurator.dir[alias]
    db = lazyconnect(uri)
    schema = create_schema(db, schema, drop=True)
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
