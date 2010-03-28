#!/usr/bin/env python
#$URL: svn+ssh://svn.mems-exchange.org/repos/trunk/quixote/setup.py $
#$Id: setup.py 26539 2005-04-11 15:47:33Z dbinger $

# Setup script for Quixote

__revision__ = "$Id: setup.py 26539 2005-04-11 15:47:33Z dbinger $"

import sys, os
from distutils import core
from distutils.extension import Extension
from ptl.qx_distutils import qx_build_py

# a fast htmltext type
htmltext = Extension(name="quixote.html._c_htmltext",
                     sources=["html/_c_htmltext.c"])

# faster import hook for PTL modules
cimport = Extension(name="quixote.ptl.cimport",
                    sources=["ptl/cimport.c"])

kw = {'name': "Quixote",
      'version': "2.0",
      'description': "A highly Pythonic Web application framework",
      'author': "MEMS Exchange",
      'author_email': "quixote@mems-exchange.org",
      'url': "http://www.mems-exchange.org/software/quixote/",
      'license': "CNRI Open Source License (see LICENSE.txt)",

      'package_dir': {'quixote':os.curdir},
      'packages': ['quixote',  'quixote.demo', 'quixote.form',
                   'quixote.html', 'quixote.ptl',
                   'quixote.server'],

      'ext_modules': [],

      'cmdclass': {'build_py': qx_build_py},
     }


build_extensions = sys.platform != 'win32'

if build_extensions:
    # The _c_htmltext module requires Python 2.2 features.
    if sys.hexversion >= 0x20200a1:
        kw['ext_modules'].append(htmltext)
    kw['ext_modules'].append(cimport)

# If we're running Python 2.3, add extra information
if hasattr(core, 'setup_keywords'):
    if 'classifiers' in core.setup_keywords:
        kw['classifiers'] = ['Development Status :: 5 - Production/Stable',
          'Environment :: Web Environment',
          'License :: OSI Approved :: Python License (CNRI Python License)',
          'Intended Audience :: Developers',
          'Operating System :: Unix',
          'Operating System :: Microsoft :: Windows',
          'Operating System :: MacOS :: MacOS X',
          'Topic :: Internet :: WWW/HTTP :: Dynamic Content',
          ]
    if 'download_url' in core.setup_keywords:
        kw['download_url'] = ('http://www.mems-exchange.org/software/files'
                              '/quixote/Quixote-%s.tar.gz' % kw['version'])

core.setup(**kw)
