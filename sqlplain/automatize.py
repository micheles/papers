import os, sys, subprocess
from sqlplain.configurator import configurator
from sqlplain.util import createdb

def collect(directory, ext):
    'Read the files with a given extension from a directory'
    sql = []
    for fname in os.listdir(directory):
        if fname.endswith(ext):
            #print 'Reading %s ...' % fname
            sql.append(file(os.path.join(directory, fname)).read())
    return sql

def makedb(alias):
    db = createdb(configurator.uri[alias], drop=True)
    testdir = configurator.dir[alias]
    schema = collect(testdir, '-schema.sql')
    data = collect(testdir, '-data.sql')
    pycode = collect(testdir, '.py')
    for chunk in schema + data:
        db.execute(chunk)
    for chunk in pycode:
        subprocess.call([sys.executable, '-c', chunk])
        
if __name__ == '__main__':
    makedb('utest')
    makedb('autest')
    makedb('ftest')
