"""
A test script to investigate the performance of bulk_insert.
"""

from __future__ import with_statement
import os, sys, csv, tempfile
from random import random
from datetime import date, timedelta
from sqlplain import do, util, table
from sqlplain.recipes import Clock

db = util.create_db('postgres_test', force=True)

CREATE_PRICE_TABLE = '''
CREATE TABLE price(
code CHAR(4),
date DATE,
price FLOAT
);
ALTER TABLE price ADD PRIMARY KEY (code, date);
'''

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

def setup():
    global fname, price
    db.execute(CREATE_PRICE_TABLE)
    price = table.DTable.object(db, 'price')
    fname = makedatafile(100, 100)
    print 'Created datafile %s' % fname

# 104 seconds for 100,000 rows on my MacBook
def test_insert():
    with clock:
        for r in csv.reader(file(fname)):
            price.insert_row(r)

# 2.4 seconds for 100,000 rows on my MacBook
def test_bulk_insert():
    db.execute('TRUNCATE TABLE price')
    with clock:
        price.bulk_insert(fname, sep=',')

def teardown():
    os.remove(fname)
