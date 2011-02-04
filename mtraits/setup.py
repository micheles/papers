try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

try:
    docfile = file('/tmp/docs.html')
except IOError: # file not found, ignore
    doc = ''
else:
    doc = docfile.read()

VERSION = '0.5.1'
setup(name='strait',
      version=VERSION,
      description='Simple Traits for Python',
      long_description="</pre>%s<pre>" % doc,
      author='Michele Simionato',
      author_email='michele.simionato@gmail.com',
      url='http://pypi.python.org/pypi/strait',
      license="BSD License",
      py_modules = ['strait'],
      keywords='',
      platforms=['any'],
      classifiers=['Development Status :: 3 - Alpha',
                   'Intended Audience :: Developers',
                   'License :: OSI Approved :: BSD License',
                   'Natural Language :: English',
                   'Operating System :: OS Independent',
                   'Programming Language :: Python',
                   'Topic :: Software Development :: Libraries'],
       zip_safe=False)
