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

The configuration file may also contain a [dir] section specifying the
full pathname for the directory containing the creational scripts of
the corresponding database.
"""

import os
from ConfigParser import RawConfigParser # no magic interpolation

class ReadOnlyObject(object):
    """
    Take a list [(name, value), ...] and returns a read-only object
    with associated attributes. The names cannot be private. The ordering
    is preserved in the list ._names. The object has a ._name attribute
    useful for debugging (the default is 'anonymous').
    """
    def __init__(self, items, name='anonymous'):
        keys = []
        for name, value in items:
            if name.startswith('_'):
                raise TypeError('Inner attributes cannot begin with "_"')
            object.__setattr__(self, name, value)
            keys.append(name)
        object.__setattr__(self, '_names', keys)
        object.__setattr__(self, '_name', name)
    def __iter__(self):
        for name in self._names:
            yield name, getattr(self, name)
    def __contains__(self, name):
        return name in self._names
    def __len__(self):
        return self(self._names)
    def __setattr__(self, name, value):
        if name in self._dic:
            raise TypeError('Read-only object!')
        else:
            object.__setattr__(self, name, value)
    def __str__(self):
        return '\n'.join('%s=%s' % (k, v) for k, v in self)
    def __repr__(self):
        return '<%s:%s>' % (self.__class__.__name__, self._name)

class _Configurator(object): # singleton
    _initialized = False
    
    def _initialize(self):
        cfp = RawConfigParser()
        self._conf_file = os.environ.get(
            'SQLPLAIN', os.path.expanduser('~/.sqlplain'))
        cfp.readfp(file(self._conf_file))
        self._conf_obj = ReadOnlyObject(
            [(sect, ReadOnlyObject(cfp.items(sect), sect))
            for sect in cfp.sections()], self._conf_file.name)
        self._initialized = True

    def __getattr__(self, name):
        if not self._initialized:
            self._initialize()
        return getattr(self._conf_obj, name)

configurator = _Configurator()
