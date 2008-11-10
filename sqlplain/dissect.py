"""
A couple of utilities to convert an uri into a pair
(driver connector, connection arguments).
"""

SUPPORTED_DBTYPES = 'mssql', 'postgres', 'sqlite'

def dissect(uri):
    """
    Extract the connection parameters from a SQLAlchemy-like uri string.
    Return a dictionary with keys
    
    - uri
    - dbtype
    - server
    - database
    - host
    - port

    In the case of mssql, the host may contain an instance name.
    """
    assert uri and isinstance(uri, str) and uri.startswith(SUPPORTED_DBTYPES), \
           'You passed an invalid uri string: %r' % uri
    dbtype, partial_uri = uri.split('://')
    if dbtype == 'sqlite': # strip a leading slash, since according to
        # SQLAlchemy conventions full_uri starts with three slashes or more
        return dict(dbtype=dbtype, user='', password='',
                    database=partial_uri[1:], uri=uri)
    elif not ('@' in partial_uri and '/' in partial_uri and ':' in partial_uri):
        raise ValueError(
            'Wrong uri %s: should be dbtype://user:passwd@host:port/db' %
            partial_uri)
    user_pwd, host_db = partial_uri.split('@')
    d = dict(dbtype=dbtype, uri=uri) # server means host:port
    d['uri'] = uri
    d['server'], d['database'] = host_db.split('/')
    d['user'], d['password'] = user_pwd.split(':')
    if ':' in d['server']: # look if an explicit port is passed
        d['host'], d['port'] = d['server'].split(':')
    else:
        d['host'], d['port'] = d['server'], None
    return d

def get_connect_params(uri):
    """
    Determine the database type (and therefore the driver to use) from
    the URI and returns the right connection factory, as well as its
    arguments user, pwd, host, port, db.
    """
    d = dissect(uri)
    driver = __import__(
        'sqlplain.%(dbtype)s_support' % d, globals(), locals(), [''])
    if d['dbtype'] == 'sqlite':    
        params = d['database']
    else:
        params = d['user'], d['password'], d['host'], d['port'], d['database']
    return driver.connect, params
