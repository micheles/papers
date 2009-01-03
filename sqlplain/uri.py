"""
A couple of utilities to convert an uri into a pair
(driver connector, connection arguments).
"""

import os
from sqlplain.configurator import configurator

SUPPORTED_DBTYPES = 'mssql', 'postgres', 'sqlite'

def imp(mod):
    return __import__(mod, globals(), locals(), [''])

class URI(object):
    """
    Extract: the connection parameters from a SQLAlchemy-like uri string.
    Has attributes

    - dbtype
    - server # means host:port
    - database
    - host
    - port
    - scriptdir
    
    In the case of mssql, the host may contain an instance name.
    """
    def __init__(self, uri):
        if isinstance(uri, URI): # copy data from uri
            vars(self).update(vars(uri))
            return
        assert uri and isinstance(uri, str), '%r is not a valid string!' % uri
        self.scriptdir = None
        if not '://' in uri: # assume it is an alias
            try:
                section = configurator.scriptdir
            except AttributeError: # missing [scripdir] section in conf
                pass
            else:
                scriptdir = getattr(section, uri, None)
                if scriptdir:
                    self.scriptdir = os.path.expanduser(scriptdir)
            try:
                uri = getattr(configurator.uri, uri)
            except AttributeError:
                msg = '%s is not a valid URI, not a recognized alias in %s' % (
                    uri, configurator._conf_file)
                msg += '; available aliases are %s' % configurator._databases
                raise NameError(msg)
        if not uri.startswith(SUPPORTED_DBTYPES):
            raise NameError('Invalid URI %s' % uri)
        dbtype, partial_uri = uri.split('://')
        if dbtype == 'sqlite': # strip a leading slash, since according to
            # SQLAlchemy conventions full_uri starts with three slashes or more
            self.dbtype = dbtype
            self.user = ''
            self.password = ''
            self.database = partial_uri[1:]
            self.host = 'localhost'
            return
        elif not ('@' in partial_uri and '/' in partial_uri and \
                  ':' in partial_uri):
            raise ValueError(
                'Wrong uri %s: should be dbtype://user:passwd@host:port/db' %
                partial_uri)
        user_pwd, host_db = partial_uri.split('@')
        self.dbtype = dbtype
        self.server, self.database = host_db.split('/')
        self.user, self.password = user_pwd.split(':')
        self.user =  self.user or os.environ.get('USER')
        if not self.user:
            raise ValueError('Empty username and $USER!')
        if ':' in self.server: # look if an explicit port is passed
            self.host, self.port = self.server.split(':')
        else:
            self.host, self.port = self.server, None

    def copy(self, **kw):
        "Returns a copy of the URI object with different attributes"
        new = self.__class__(self)
        vars(new).update(kw)
        return new

    def import_driver(self):
        "Import the right driver and populate the util module"
        from sqlplain import util
        dbtype = self.dbtype
        driver = imp('sqlplain.%s_support' % dbtype)
        driver_util = imp('sqlplain.%s_util' % dbtype)
        # dynamically populate thw 'util' module with the driver-specific func
        for name, value in vars(driver_util).iteritems():
            if name.endswith(dbtype):
                setattr(util, name, value)
        return driver
    
    def get_driver_connect_params(self):
        """
        Determine the database type (and therefore the driver to use) from
        the URI and returns the right connection factory, as well as its
        arguments user, pwd, host, port, db.
        """
        driver = self.import_driver()
        if self.dbtype == 'sqlite':    
            params = self.database
        else:
            params = (self.user, self.password, self.host,
                      self.port, self.database)
        return driver.dbapi2, driver.connect, params

    def __getitem__(self, name):
        # make the interpolation syntax (string-templ % self) possible
        return self.__dict__[name]
    
    def __str__(self):
        if self.dbtype == 'sqlite':
            return 'sqlite:///' + self.database
        t = '%(dbtype)s://%(user)s:xxxxx@%(server)s/%(database)s'
        return t % self
