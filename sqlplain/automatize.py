import os
from sqlplain.configurator import configurator
from sqlplain.util import createdb

def makedb(alias):
    db = createdb(configurator.uri[alias], drop=True)
    testdir = configurator.dir[alias]
    for fname in os.listdir(testdir):
        if fname.endswith('.sql'):
            code = file(os.path.join(testdir, fname)).read()
            db.execute(code)

    
if __name__ == '__main__':
    makedb('ftest')
