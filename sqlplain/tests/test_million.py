"""
A test script to investigate the performance of load_file.
"""

from __future__ import with_statement
import os, sys, csv, tempfile
from random import random
from datetime import date, timedelta
from sqlplain import do, util, table
from sqlplain.recipes import Clock

create_price_table = do('''
CREATE TABLE price(
code CHAR(4),
date DATE,
price FLOAT,
PRIMARY KEY (code, date)
)
''')

clock = Clock(lambda et: sys.stdout.write('Elapsed time: %s\n' % et))

def makedatafile(ncodes, ndates):
    "Create a big datafile with ncodes * ndates rows; return the file name"
    tmp, fname = tempfile.mkstemp()
    today = date.today()
    for i in range(ncodes):
        for j in range(ndates):
            code = '%04d' % i
            day = today - timedelta(j)
            value = 1 + random()
            os.write(tmp, '%s,%s,%s\n' % (code, day, value))
    os.close(tmp)
    os.chmod(fname, 0755)
    return fname

def makedb(uri):
    global fname
    db = util.create_db(uri, force=True)
    create_price_table(db)
    fname = makedatafile(100, 100)
    print 'Created datafile %s' % fname
    return db

def test():
    db = makedb('sqlite_test')
    price = table.DTable.reflect(db, 'price')
    with clock:
        # 104 seconds for 100,000 rows on my MacBook
        for r in csv.reader(file(fname)):
            price.insert_row(r)
        yield lambda x:x, 'insert_row'
    price.truncate()
    with clock:
        # 2.4 seconds for 100,000 rows on my MacBook
        price.load_file(fname, sep=',')
    yield lambda x:x, 'load_file'
    
def teardown():
    os.remove(fname)
