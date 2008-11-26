import psycopg2 as dbapi2

ISOLATION_LEVELS = None, 0, 1, 2

def connect(params, isolation_level=None, **kw):
    user, pwd, host, port, db = params
    port = port or 5432
    conn = dbapi2.connect(
        database=db, host=host, port=port, user=user, password=pwd, **kw)
    if isolation_level is None:
        conn.set_isolation_level(0)
    return conn
