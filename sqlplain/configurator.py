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

class ConfigObj(UserDict.DictMixin):
    # spaces are ignored and options are case-insensitive
    """Convert a configuration file or string into a dictionary-like object
    sections -> items. For instance
    
    cfg = ConfigObj('''
    [fields]
    title: char(32) 
    score: char(6)
    author: char(32)
    date: char(8)
    genre: char(4)
    nation: char(4)
    ''')
    print cfg.fields
    """
    def __init__(self, config_str_or_file):
        if isinstance(config_str_or_file, basestring):
            # dedent and convert into a file-like object
            cfg = StringIO(textwrap.dedent(config_str_or_file))
        else:
            cfg = config_str_or_file # assume cfg is a file-like object\
        try:
            cfp = RawConfigParser()
            cfp.readfp(cfg)
            self._keys = []
            for sect in cfp.sections():
                setattr(self, sect, dict(cfp.items(sect)))
                self._keys.append(sect)
        finally:
            cfg.close()
            
    def iteritems(self):
        for k in self._keys:
            yield k, getattr(self, k)
            
    def keys(self):
        return self._keys
    
    def textrepr(self):
        for sect, settings in self.iteritems():
            yield '\n[%s]\n' % sect
            for k, v in settings.iteritems():
                yield '%s=%s' % (k, v)
                
    def save(self, fname):
        f = file(fname, 'w')
        for line in self.textrepr():
            print >> f, line
        f.close()
            
class _Configurator(object): # singleton
    initialized = False
    
    def _initialize(self):
        self.conf_file = os.environ.get(
            'SQLPLAIN', os.path.expanduser('~/.sqlplain'))
        self.conf_obj = ConfigObj(file(self.conf_file))
        self.uri = self.conf_obj['uri']
        self.initialized = True

    def get_uri(self, alias):
        if not self.initialized:
            self._initialize()
        return self.uri[alis]
    
configurator = _Configurator()
