try: # Python 2.5
    from sqlite3 import dbapi2
except ImportError: # Python < 2.5
    from pysqlite2 import dbapi2

def connect(fname, autocommit=True, **kw):
    if autocommit:
        level = None
    else:
        level = ''
    dbapi2.register_converter('datetime', dbapi2.converters['TIMESTAMP'])
    return dbapi2.connect(
        fname, isolation_level=level, detect_types=dbapi2.PARSE_DECLTYPES, **kw)
