from __future__ import with_statement
import os
from sqlplain import lazyconnect, util
from test_million import makedb, makedatafile, clock, create_price_table

databases = 'postgres_test sqlite_test'.split()


def test_truncate():
    for uri in databases:
        db = util.create_db(uri, force=True)
        create_price_table(db)
        fname = makedatafile(100, 100)
        try:
            util.load_file(db, fname, 'price')
            with clock:
                util.truncate_table(db, 'price')
            yield lambda *a: None, uri, 'truncate'
            
            util.load_file(db, fname, 'price')
            with clock:
                db.execute('delete from price')
            yield lambda *a: None, uri, 'delete'
        finally:
            os.remove(fname)
