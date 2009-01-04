try:
    from setuptools import setup
except ImportError:
    from distutils.core import setup

VERSION = '0.5.0'
setup(name='strait',
      version=VERSION,
      description='Simple Traits for Python',
      long_description="""strait is a simple implementation of trait-based object system for Python
""",
      author='Michele Simionato',
      author_email='michele.simionato@gmail.com',
      url='http://www.phyast.pitt.edu/~micheles/python/strait.html',
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
