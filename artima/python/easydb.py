# easydb.py
from operator import itemgetter
from collections import namedtuple # for Python >= 2.6

def get_table_from_db(cursor, query_templ, query_args=(), ntuple=None):
    if query_args:
        cursor.execute(query_templ, query_args)
    else:
        cursor.execute(query_templ)
    rows = cursor.fetchall()
    fields = map(itemgetter(0), cursor.description)
    Ntuple = ntuple or namedtuple('DBTuple', fields)
    yield Ntuple(*fields)
    for row in rows:
        yield Ntuple(*row)

if __name__ == '__main__': # test
    from sqlite3 import dbapi2
    conn = dbapi2.connect(':memory:')
    conn.execute('create table test(id integer, descr varchar)')
    conn.execute("insert into test values (1,'one')")
    conn.execute("insert into test values (2,'two')")
    for rec in get_table_from_db(conn.cursor(), 'select * from test'):
        print rec
