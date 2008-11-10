import psycopg2

def connect(params, autocommit=True, **kw):
    user, pwd, host, port, db = params
    port = port or 5432
    conn = psycopg2.connect(
        database=db, host=host, port=port, user=user, password=pwd, **kw)
    if autocommit:
        conn.set_isolation_level(0)
    return conn
