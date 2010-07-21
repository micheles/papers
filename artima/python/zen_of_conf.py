r"""
The Zen of configuring Python applications
==============================================================

:Author: Michele Simionato
:Email: michele.simionato at gmail.com
:Initial-date: 26-Aug-2007
:Revision-date: 15-Dec-2007

.. contents::

Abstract
-----------

At first sight, the task of configuring a Python application looks
rather trivial. However, in real life, it can become rather annoying
and sometimes even daunting.
The issue is more sociological than technical. The root of all evil 
stands in the fact that the language does
not provide a standard mechanism to configure applications, so everybody
is forced to reinvent the wheel in different ways, at all level of
sophistication. In this short note, I will present some ideas of mine
about what can be done to solve the configuration problem in
Python applications.

The current situation
---------------------------------------------------------

First of all, let me split the issue of configuring an application
into two separated issues: 

 1. the issue of the format of the configuration data (usually
    configuration files);

 2. the issue of passing the configuration data to the application, 
    i.e. the startup mechanism.

As of now, the standard library provides partial support for the first
issue and no support at all for the second one.  The support for
configuration data is in the ConfigParser_ module, which is able to
read files in .INI format. The availability of this standard module made many
projects to choose .INI files are their configuration format. However
.INI files are rather limited and while they work pretty well for
small projects, you will feel the need for something more powerful in
larger projects. Because of this need, .INI files on steroids have
appeared. For instance PasteDeploy_ uses .INI files where section names
may have colons (i.e. [main:app], [main:conf]). 
On the other hand ConfigObj_, an
extension of ConfigParser, allows to define hierarchical configuration
files, by using a multiple brackets notation ([[nested-section]]).
One may argue that these are abuses of .INI files, which by their
nature are intended to be non-hierarchical. One may also argue that
the right format for hierarchical configuration files is XML (but
many disagree with that position). For instance the `Zope 3`_
people are using XML configuration files, even if most Pythonista
hate them because they thought they escaped from
the hell of Java enterprise configuration
nightmares.  Many people also are dissatisfied with any existing
configuration file format and want to impose to their users
their own custom format, even spending time to implement a parser for
it. Finally other people, more pragmatic, just advocate to use Python
files are configuration files. Finally, in some special cases,
you do not want to have any configuration data at all, and
you want to keep your configuration data in a database, in
environment variables or somewhere else.

I will not spend much time of the
issue of the configuration data format: I will focus instead on the 
neglected issue of passing the configuration to the
application, i.e. the startup mechanism.

.. _ConfigParser: http://docs.python.org/lib/module-ConfigParser.html
.. _PasteDeploy: http://pythonpaste.org/deploy/
.. _Zope 3: http://www.zope.it/info/Zope/zope-3

Launching applications
------------------------------------------------------

What does happen when you launch a Python application from the command line?
It is well known: the shell parses the command line arguments and pass
them to Python through the ``sys.argv`` list, ready for further
processing. Unfortunately, when you have more than two arguments it
becomes rather tedious to type them all; it is more convenient to store
them in a configuration file and to pass to Python just the file name.
That looks easy, but it is not because:

1. you have to decide the format of the configuration file;
2. depending on the format, you have to import in your script the
   right parser;
3. you have to decide how to pass the name of the configuration file to
   the application;
4. you have to write some glue code in order to pass the parsed configuration
   data to your application, and that glue code depends from the application
   as well from the configuration data.

All these choices are left to the single programmer, are not standard, and
must be repeated for each single application you write. This is just boring
and painful, since it does not have to be that way.
So I will suggest what Python should do (according to my own opinionated
view) and I will show what we can do meanwhile.

In my view, standard Python should grow a command option ``-a`` (*-a* is
a mnemonic for *application*)
such that

  ``$ python -a myapp myconf``

will run the application ``myapp`` by passing to it a *configuration
object* named ``myconf``. Configuration objects would be read from a
configuration file. In order to make things simple let us assume for
the moment that the configuration file is just a Python module and
that the configuration objects are just plain dictionaries.  For a
typical Web application the configuration file could be something like
the following::

 $ cat ~/.myapprc
 prod_conf = {
    'host' : 'myhost.com',
    'port': '80',
    'dsn' : 'postgres://user:pwd@db_host/production_db',
    }
 test_conf = {
    'host' : '',
    'port': '8080',
    'dsn' : 'postgres://user:pwd@db_host/testing_db',
    }

That configuration file defines two configuration dictionaries 
containing the right parameters to run the application in production
and testing environments. The names of the dictionaries could end
with ``_conf`` so that Python could easily recognize them and display
a meaningful message in case you don't specify the configuration
object, such as::

 $ python -a myapp
 Application configuration not specified. You may choose between 
 prod_conf, test_conf, dev_conf

``myapp`` here is the absolute Python "dotted" path of the application (for
instance ``mypackage.myapp``). If ``myapp`` is a
module, it should have a ``main`` procedure taking a single argument,
the configuration object; if ``myapp`` is a package, its
``__init__.py`` submodule should have a ``main`` procedure taking the
configuration object as argument.

While waiting
-----------------------------

Of course, growing a new switch for the Python interpreter is not an easy
job. The idea should be rephrased as a PEP, there should be a lot
of discussion, finally a pronouncement and even in the case of a positive
outcome you will have to wait for years before having the feature in
core Python. For the impatient, I give in appendix a simple module
implementing the configuration mechanism I suggested before.
The module is called ``call_with_conf`` and it is intended to
be called together with the ``-m`` switch (available from Python 2.4+).

The resolution of names implemented in ``call_with_conf`` is the following:

1. first, check if the environment variable ``${APPNAME}_STARTUP`` is set
   to a pathname and look for the requested configuration object
   in that file; if none is found, go to step 2;

2. look if a file ``~/{APPNAME}.ini`` exists in the user
   home directory and search for the requested configuration object
   in that file; if none is found, go to step 3;

3. look if a file ``~/{APPNAME}rc`` exists in the user
   home directory and search for the requested configuration object
   in that file; if none is found, go to step 4;

4. look at the application namespace for available configurations;
   if the requested configuration is found, use it, else
   abort the application with an explicative error message.

The algorithm ensure that user configurations have the precedence over
system configurations.
Consider for instance the following example application:

$$example

The application is a simple module which for sake of commodity
contains the configuration object used in development.
The production and testing configuration are stored in an
``~/.examplerc`` file. An user can define its own ``dev_conf``
dictionary in ``~/.examplerc`` without needing to edit ``example.py``.

The application should be run as::

 $ python -m call_with_conf example dev_conf
 dsn postgres://user:pwd@localhost/development_db
 host localhost
 port 8080

If you forget to specify the configuration parameter, you get an handy
message::

 $ python -m call_with_conf example   
 A trivial application exemplifying the call_with_conf configuration mechanism
 usage:$ python -m call_with_conf example [dev_conf|test_conf|prod_conf]

Of course, you will end up by giving a shorter name to
``python -m call_with_conf``::

  $ alias cwc="python -m call_with_conf"

Some applications are meant to be run by an user without a home directory
(such as the 'nobody' user in many Unix systems); for those applications
the location of the configuration file should be specified via the
environment variable.

Compatibility with pre-existing code
---------------------------------------------------------------

In order to use ``call_with_conf`` with existing applications
you will need to write some glue code. For instance, if you have an
existing application expecting an .INI file, you may need some code to
convert a dictionary of dictionaries into an .INI file, such as

$$dict2ini

Then, if your old application had a ``runapp`` function expecting an .INI
file, you can just define a ``main`` function such as

$$main

Notice that there is actually a *good argument for preferring .INI
files over dictionaries*:
if your configuration file can be manually edited by users not knowing
Python and the configuration file contains Windows pathnames, then
you are certainly better off with an .INI file, since the users will definitely
get wrong the escaping of the backslash required by Python strings. On the
other hand, typically in an end user Windows application the configuration
parameters are edited from a GUI, not manually, so this case is pretty rare
in practice.

In my experience, there is lot of code out there using the idiom

$$example2

To make it possible to run such code, ``call_with_conf`` can be invoked without
specifying the configuration name;  then the ``main`` routine will be called 
without arguments. Here is an example::

 $ cwc example2
 called main()

Moreover, it is possible to specify the routine to run by adding its name
(prefixed by colons) to the application name.
For instance, if you want to run the ``test`` function instead of the ``main``
function, you may just give::

 $ cwc example2:test
 called test()

That also works for real configurations, if the called routines accept a
dictionary argument.

Discussion
-----------------------------------------

The first thing I want to point out is that the ideas I am proposing here are
not particularly smart or innovative. I am sure everybody who has spent
time thinking about configuring an application has come out with something
similar. For instance the Paste_ configuration mechanism has 
many similarities with the approach I am proposing (Paste has the concept of application
factory depending on a configuration dictionary). My point in writing this article
is to convince the powers that be that we need a common configuration mechanism 
in core Python: it does not need to by my mechanism, any mechanism would be better 
than the current situation.

Secondly, a point about which I have no clear opinion is 
if we should restrict the choice of configuration objects
to dictionaries. Dictionaries have many advantages:

1. everybody understand dictionaries;

2. dictionaries can be nested, so you have the full power of XML 
   configuration files, and actually more;

3. if you guarantee that the ``main`` routine will get a dictionary, it will be
   much easier to write uniform, configuration format-independent ``main``
   routines (in the same sense that WSGI restricts the parameter 'environ' to 
   be a dictionary and not a generic object).

4. it is always easy to get the dictionary of an object using vars,  so if 
   you really want to use a configuration object you can;

5. you may pretend that the configuration files are not Python, but JSON,
   so you get the benefit of using cool acronyms ;)

6. if the configuration files are intended to be edited by Python programmers,
   you can still use the full power of Python.

Suppose for instance you want to change the port number on the test
configuration. You don't need to fully rewrite the configuration,
you can just do

::

 test2_conf = test_conf.copy()
 test2_conf['port'] = 8081
 
which should be fully self-explicative even for people with no knowledge of
Python at all. Moreover you can give sensible names to dictionaries and
write very concise and readable configuration files even in cases of
complex nesting. 

On the other hand, using objects more general than dictionaries may
have advantages too. In particular, one could also use as
configuration objects dict-like objects (i.e. instances of
DictMixin_). For instance, to be sure that configuration
parameters are not changed at runtime, one could use an ImmutableDict
subclass that disallows the ``__setitem__`` method.  Another possible
choice is to use classes as configuration objects, since that would
give for free inheritance of configurations, which may be a good thing
or a bad thing too. For the moment I would not restrict the choice of
the configuration objects.

Another debatable point is the choice of the configuration file extension.
Right now I am using the extension ``rc``. Some
will argue that we need a more specific extension. Others will certainly
advocate a ``.py`` extension. I am definitely -1 on the ``.py`` extension:
I am on the camp of people who believe that configuration files should
be kept separated from code, at least conceptually. Here, the fact that
they are actually
valid code is an implementation detail: an useful one, for sure, but still
an implementation detail. In the common case, users should not put any logic 
in the configuration files, and not having a ``.py`` extension make that
clear; on the other hand, for the cases where it makes sense, users *can*
put anything they want in the configuration files. We are all consentient
adults, right?
And in any case one may turn on syntax highlighting even for files without
a ``.py`` extension, and this is about the only advantage I can think of
for having a ``.py`` extension.

Finally, there is a point about security. Since configuration files are
executable Python files, they can do everything including formatting
your hard disk. However, this is a non-issue. It is the user that is in
charge of writing the configuration file and he has already the power to
perform damages if he wants. There is always
the option to hide the configuration files and to write a GUI to edit them,
if you do not trust your users.

.. _Paste: http://pythonpaste.org
.. _DictMixin: http://docs.python.org/lib/module-UserDict.html

Appendix: the *call_with_conf* module
---------------------------------------

Just cut and paste the following code in a file called *call_with_conf.py*
in your Python path. You are free to extend and improve the recipe to
suit your needs.

$$call_with_conf

----

     *Before you study Zen, mountains are mountains and rivers are rivers. 
     While you are studying Zen, mountains are no longer mountains and rivers 
     are no longer rivers. Once you have had enlightenment, mountains are once 
     again mountains and rivers again rivers.*

"""

import ConfigParser, call_with_conf, example, cStringIO, example2

def ini2dict(fp, Parser=ConfigParser.RawConfigParser):
    parser = Parser()
    parser.readfp(fp)
    return parser._sections

def dict2ini(nested_dic, Parser=ConfigParser.RawConfigParser):
    fp = cStringIO.StringIO()
    parser = Parser()
    parser._sections = nested_dic
    parser.write(fp)
    fp.reset()
    return fp

def runapp(inifile):
    'do something'

def main(conf):
    runapp(dict2ini(conf))

import re, inspect, datetime
DDOLLAR = re.compile(r'\$\$([\w_]+)')
TODAY = datetime.date.today().isoformat()[:10]

def create_rst(pyfile):
      import os
      pyname, ext = os.path.splitext(os.path.basename(pyfile))
      mod = __import__(pyname)
      fname = pyfile.replace('.py', '.txt')

      def repl(mo):
          obj = getattr(mod, mo.group(1))
          if isinstance(obj, str):
                return obj
          lines = inspect.getsourcelines(obj)[0]
          return '.. code-block:: python\n\n' + '\n'.join(
                ' ' + ln.rstrip() for ln in lines)

      open(fname, 'w').write(DDOLLAR.sub(repl, mod.__doc__))
      
if __name__ == '__main__':
    import doctest; doctest.testmod()
    import sys; create_rst(sys.argv[0])
