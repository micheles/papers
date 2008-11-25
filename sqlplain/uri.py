"""
A couple of utilities to convert an uri into a pair
(driver connector, connection arguments).
"""

import os
from sqlplain.configurator import configurator

SUPPORTED_DBTYPES = 'mssql', 'postgres', 'sqlite'

class URI(dict):
    def __init__(self, uri):
        """
        Extract the connection parameters from a SQLAlchemy-like uri string.
        Return a dictionary with keys

        - uri
        - dbtype
        - server # means host:port
        - database
        - host
        - port

        In the case of mssql, the host may contain an instance name.
        """
        if isinstance(uri, URI): # do nothing
            self.update(uri)
            return
        assert uri and isinstance(uri, str), '%r is not a valid string!' % uri
        if not '://' in uri: # assume it is an alias
            try:
                uri = configurator.uri[uri]
            except KeyError:
                raise NameError(
                    '%s is not a valid URI, not a recognized alias' % uri)
        if not uri.startswith(SUPPORTED_DBTYPES):
            raise NameError('Invalid URI %s' % uri)
        dbtype, partial_uri = uri.split('://')
        if dbtype == 'sqlite': # strip a leading slash, since according to
            # SQLAlchemy conventions full_uri starts with three slashes or more
            self['dbtype'] = dbtype
            self['user'] = ''
            self['password'] = ''
            self['database'] = partial_uri[1:]
            return
        elif not ('@' in partial_uri and '/' in partial_uri and \
                  ':' in partial_uri):
            raise ValueError(
                'Wrong uri %s: should be dbtype://user:passwd@host:port/db' %
                partial_uri)
        user_pwd, host_db = partial_uri.split('@')
        self['dbtype'] = dbtype
        self['server'], self['database'] = host_db.split('/')
        self['user'], self['password'] = user_pwd.split(':')
        self['user'] =  self['user'] or os.environ.get('USER')
        if not self['user']:
            raise ValueError('Empty username and $USER!')
        if ':' in self['server']: # look if an explicit port is passed
            self['host'], self['port'] = self['server'].split(':')
        else:
            self['host'], self['port'] = self['server'], None

    def copy(self, **kw):
        new = self.__class__(self)
        new.update(kw)
        return new
    
    def get_driver_connect_params(self):
        """
        Determine the database type (and therefore the driver to use) from
        the URI and returns the right connection factory, as well as its
        arguments user, pwd, host, port, db.
        """
        module = 'sqlplain.%(dbtype)s_support' % self
        driver = __import__(module, globals(), locals(), [''])
        if self['dbtype'] == 'sqlite':    
            params = self['database']
        else:
            params = (self['user'], self['password'], self['host'],
                      self['port'], self['database'])
        return driver.dbapi2, driver.connect, params
        
    def __str__(self):
        if self['dbtype'] == 'sqlite':
            return 'sqlite:///' + self['database']
        t = '%(dbtype)s://%(user)s:%(password)s@%(server)s/%(database)s'
        return t % self
