import os, tempfile
from random import random
from datetime import date, timedelta
from sqlplain import do, util, table

db = util.create_db('postgres_test', force=True)

CREATE_PRICE_TABLE = '''
    CREATE TABLE price(
    code CHAR(4),
    date DATE,
    price FLOAT
    );
    ALTER TABLE price ADD PRIMARY KEY (code, date);
'''

insert_price = table.insert_row('price', 'code date price')

rows, fname = [], ''
    
def create_values(ncodes, ndates):
    global fname
    tmp, fname = tempfile.mkstemp()
    today = date.today()
    for i in range(ncodes):
        for j in range(ndates):
            code = '%04d' % i
            day = today - timedelta(j)
            value = 1 + random()
            rows.append((code, day, value))
            os.write(tmp, '%s,%s,%s\n' % (code, day, value))
    os.close(tmp)
    return rows

def setup():
    db.execute(CREATE_PRICE_TABLE)
    create_values(10, 10)

def test_insert():
    for row in rows:
        insert_price(db, row)

def test_bulk_insert():
    do('TRUNCATE TABLE price')
    util.bulk_insert(db, file(fname), 'price', sep=',')
