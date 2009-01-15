try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

VERSION = '0.1.0'

## DEPENDENCIES decorator

if __name__ == '__main__':
    try:
        docfile = file('/tmp/documentation.html')
    except IOError: # file not found, ignore
        doc = ''
    else:
        doc = docfile.read()
    setup(name='sqlplain',
          install_requires = [
        'decorator>=3.0.0'],
          version=VERSION,
          description='Keep SQL simple',
          long_description='</pre>%s<pre>' % doc,
          author='Michele Simionato',
          author_email='michele.simionato@gmail.com',
          url='http://pypi.python.org/pypi/sqlplain',
          license="BSD License",
          packages = ['sqlplain'],
          keywords="SQL DB API 2",
          platforms=["All"],
          classifiers=['Development Status :: 1 - Alpha',
                       'Intended Audience :: Developers',
                       'License :: OSI Approved :: BSD License',
                       'Natural Language :: English',
                       'Operating System :: OS Independent',
                       'Programming Language :: Python',
                       'Topic :: Software Development :: Libraries',
                       'Topic :: Utilities'],
          zip_safe=False)

