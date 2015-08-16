import ast
try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup
try:
    docfile = open('/tmp/docs.rst')
except IOError:  # file not found, ignore
    doc = ''
else:
    doc = docfile.read()
    docfile.close()

firstline = next(open('strait.py'))  # extract the version string
VERSION = ast.literal_eval(firstline.split('=')[1].strip())

setup(name='strait',
      version=VERSION,
      description='Simple Traits for Python',
      long_description=doc,
      author='Michele Simionato',
      author_email='michele.simionato@gmail.com',
      url='http://pypi.python.org/pypi/strait',
      license="BSD License",
      py_modules=['strait'],
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
