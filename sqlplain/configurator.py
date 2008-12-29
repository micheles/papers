"""
This module defines a singleton configurator object with a single
public method .get_uri(alias).
When .get_uri is called, the environment variable $SQLPLAIN is looked at.
It should contain the name of a configuration file. If $SQLPLAIN is missing,
the configuration file name is assumed to be ~/.sqlplain. The configuration
file must exists and must contain a section [uri] as follows:

[uri]
dev: mssql://user:passwd@host:port/dev_db
test: mssql://user:passwd@host:port/test_db
prod: mssql://user:passwd@host:port/prod_db

Then .get_alias('dev') returns the URI mssql://user:passwd@host:port/dev_db.
Analogously for 'test' and 'prod'.
"""

import os
import UserDict
from ConfigParser import RawConfigParser # no magic interpolation
from cStringIO import StringIO

class ReadOnlyObject(object):
    def __init__(self, items):
        keys = []
        for name, value in items:
            if name.startswith('_'):
                raise TypeError('Inner attributes cannot begin with "_"')
            object.__setattr__(self, name, value)
            keys.append(name)
        object.__setattr__(self, '_dic', dic)
        object.__setattr__(self, '_keys', keys)
    def __iter__(self):
        for name in self._keys:
            yield name, getattr(self, name)
    def __contains__(self, name):
        return name in self._keys
    def __len__(self):
        return self(self._keys)
    def __setattr__(self, name, value):
        if name in self._dic:
            raise TypeError('Read-only object!')
        else:
            object.__setattr__(self, name, value)
    def __str__(self):
        return '\n'.join('%s=%s' % (k, v) for k, v in self)

class _Configurator(object): # singleton
    _initialized = False
    
    def _initialize(self):
        cfp = RawConfigParser()
        self._conf_file = os.environ.get(
            'SQLPLAIN', os.path.expanduser('~/.sqlplain'))
        cfp.readfp(file(self._conf_file))
        self._conf_obj = ReadOnlyObject(
            (sect, ReadOnlyObject(cfp.items(sect)))
            for sect in cfp.sections())
        self._initialized = True

    def __getattr__(self, name):
        if not self._initialized:
            self._initialize()
        return getattr(self._conf_obj, name)

configurator = _Configurator()
