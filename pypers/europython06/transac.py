from __future__ import with_statement
from contextlib import contextmanager, closing

@contextmanager
def cursor(conn, action='commit'):
    assert action in ('commit', 'rollback')
    curs = conn.cursor()
    try:
        yield curs
    except: 
        conn.rollback()
        raise
    finally:
        curs.close()
    getattr(conn, action)() # commit or rollback
        
if __name__ == '__main__':
    import psycopg
    with closing(psycopg.connect('')) as conn:
        with cursor(conn, 'rollback') as curs:
            curs.execute('create table example (name varchar(32))')
            curs.execute("insert into example values ('pippo')")
            curs.execute("insert into example values ('lippo')")
            curs.execute('select * from example')
            print curs.fetchall()

