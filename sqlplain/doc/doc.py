"""
SQLPLAIN, an opinionated database library

.. contents::

Introduction
---------------------------------------------------------------

I never liked the database API for Python. I have always found it cumbersome
to use and very much unpythonic. Moreover, it is too much low level for my
taste. So I have always used some small custom made wrapper over it.
Then, SQLAlchemy came. SQLAlchemy has a lot of good things going for it,
but I must say that I felt it too much high level for my taste, so I kept
using my own little wrapper over the DB API 2. Recently at work we
decided to make SQLAlchemy the official database toolkit for our code.
Then I have decided to remove my own little wrapper from our code
base. Since in the years I have developed some affection for it I have
decided to clean it up a little and to give to it a new life as an
Open Source library.

``sqlplain`` is intended to be a lightweight wrapper over the DB API 2
and *not* an Object Relation Mapper. Currently the code base of sqlplain
consists of nearly 1,000 lines of code (for comparison, sqlalchemy 0.5 contains
something like 42,000 lines of code). In the future sqlplain may grow,
but I am committed to never make it "big" in any sense, and I would like
to keep it well below the 5% of the size of sqlalchemy. The reason is that
one of the main design goals behind sqlplain is to keep it small enough
that anybody can read the full code base in a single day and have a good
idea of how it works. Moreover, the code base is intentionally kept simple,
and no fancy Python features are used: it just requires Python 2.4 to run.

The name ``sqlplain`` come from the main design goal of the library:
to use plain old SQL as much as possible, and not Python objects.
This is the reason why ``sqlplain`` will never become an Object Relational
Mapper. ORM have they usages: in particular they are useful if your main
goal is to persist Python objects into a relation database. However,
this is not the use case for ``sqlplain``. ``sqlplain`` is intented to
be used in situations where you have a pre-existing relational database
which has an independent life from your Python application.

Nowadays it has become common to embed the model in Python code and to
abstract completely from the database; this is the way Diango works
(see the `Django philosophy`_ page) and the same is true for most
modern Web framework. That approach is fine in many circumstances: it
basically amounts to hiding the underlying relation database and to
use it as if it was an object database. However ``sqlplain`` does not
follow this route. ``sqlplain`` does not want to hide the underlying
database: I routinely have to debug performance issue and therefore I
want to have in the code plain SQL queries that I can cut and paste on
the database consolle to run them under the query analyzer.

When you are debugging performance issues, you want to stay as close
as possible to bare SQL, since you don't want to be confused about the
performance issue due to the ORM and the intrinsic issues: this is
the reason why ``sqlplain`` has very little support for generating
SQL programmatically from Python objects. This may change and I could
improve the support for SQL generation in future versions; however,
it is very unlikely that ``sqlplain`` will ever support creating tables or
other database objects from Python objects.
There is nothing similar to sqlalchemy ``Metadata.create_all``: if
you want to change the schema of your database you must do it via
plain old SQL queries, without any support from Python.

``sqlplain`` is very much opinionated

.. _Django philosophy: http://docs.djangoproject.com/en/dev/misc/design-philosophies/#misc-design-philosophies

sqlplain for the impatient
---------------------------------------------------------------

Enough with the introduction: here is how you can run a query on a
given database via ``sqlplain``. For exemplification purposes, let
me assume that you have a database of books (``bookdb``) running on localhost
on SQL Server, and a superuser ``pyadmin`` with password ``secret`` which
all permissions on the ``bookdb``. Suppose the database contains a table
called ``book`` containing books with title, author, publication date,
and other information. Suppose I want to retrieve all the books by Isaac
Asimov. That can be done with the following code:

.. code-block:: python

 >> from sqlplain import lazyconnect
 >> bookdb = lazyconnect('mssql://pyadmin:secret@localhost/bookdb')
 >> bookdb.execute("SELECT * FROM book WHERE author LIKE ?", ('%Asimov%',))

Here is the explanation. The first line import the ``LazyConnection``
class from ``sqlplain``: instances of ``LazyConnection`` are *lazy
connection* objects.  A lazy connection is a wrapper over an ordinary
DB API 2 connection: the connection is lazy in the sense that the real
DB API 2 connection is instantiated only when needed, i.e. when the
``execute`` method is called. Just instantiating ``bookdb`` does not
open any connection: however instantiating ``LazyConnection`` involves
parsing the connection string or uri (in this case
``mssql://pyadmin:secret@localhost/bookdb``) and importing the
corresponding database driver (in this case ``pymssql``) which must be
installed in your system, otherwise you get an ``ImportError``.

The syntax of the URI is the same as in SQLAlchemy (I did copy from
SQLAlchemy; even Storm uses the same convention and I see no reason
to change it). Internally ``LazyConn`` instantiates an URI object
which is a dictionary:

.. code-block:: python

 >> bookdb.uri
 {'database': 'bookdb',
 'dbtype': 'mssql',
 'host': 'localhost',
 'password': 'secret',
 'port': None,
 'server': 'localhost',
 'user': 'pyadmin'}

The port is None here, therefore the low level driver ``pymssql`` will open
the connection by using the default port number for MS SQL, i.e. 1433.

The ``execute`` method of the lazy connection object is the one performing
the real job: it opens a low level connection, instantiates a DB API 2
cursor and it runs the ``SELECT`` query: the result is returned as a list
of named tuples. Named tuples are a Python 2.6 construct, however ``sqlplain``
ships with its own version of namedtuples (I have just copied Raymond
Hettinger's recipe from the Python Cookbook site) which is used if
you are running an early version of Python.

An important thing to notice is that ``sqlplain`` uses the so-called
qmark param-style, i.e. the place holder for parametric queries is
the question mark ``?`` for *all* supported database drivers, even
if the underlying low level driver uses the pyformat param-style
(both pymssql and psycogpg uses the pyformat param-style, which is
a terrible choice, since it collides for the usage of ``%s`` in Python
string templates).

The ``execute`` method is smart enough: if you run it again,
the previously instantiated DB API2 connection and cursors are
re-used, i.e. it does not recreate a connection for each query.
You can access the low level connection and cursor via the
properties ``._conn`` and ``._curs``:

.. code-block:: python

 >> bookdb._conn

 >> bookdb._curs

There is an underscore, meaning that you are not supposed to access
those attributes directly. Notice that calling twice
``bookdb._conn``/``bookdb._curs`` may return different connections/cursors
in rare circumstances.

You can ``execute`` a ``SELECT`` query, or other types of queries, such
as ``UPDATE/INSERT/DELETE``; in those cases ``execute`` does not return
a list of named tuples, it returns a number instead:

.. code-block:: python

 >> bookdb.execute("UPDATE book SET author=? WHERE author like ?",
     ('Isaac Asimov', '%Asimov%'))
 2

The number is the DB API 2 ``rowcount`` attribute of the cursor, i.e.
the number of rows affected by the query.

Allocated connections take resources on the server even if
they are not used, therefore you may want to close an unused connection:


.. code-block:: python

 >> bookdb.close()

Notice that closing twice a connection will
not raise an error.

Any closed connection will be automatically re-reopened at the first
call of ``.execute``. 

In Python 2.5+ you can use the ``with`` statement and the
``contextlib.closing`` function to make sure a connection is closed
after the execution of a given block of code, by using the pattern

.. code-block:: python
 
  with closing(cx):
      do_something(cx)

Lazy connections have an interesting feature: if
a query raises an error, ``sqlplain`` tries to execute it a second
time with a fresh connection, and sometimes the second attempt may
succeed. The reason is that
sometimes a correct query fails due to network issues or other
problems (for instance somebody restarted the database server and
the existing connection has become obsolete) which are transitory:
so the first time the query fails because the connection is
in a bad state, but the second time it succeeds since the fresh
connection is in a good state. Of course, if the network outage or
the other error persists, there will be an error even at the second
attempt and the exception will be raised (we all know that
*errors should never pass silently*). By default this feature is
turned on, but you may disable it (if you do not want to retry
every failed query) by setting the ``.retry`` attribute
of the lazy connection object (or class) to ``False``.

Retrying connections are good when writing long running programs, such as
user interfaces (CLI, GUI and Web): in such a situations
errors are trapped.

The configuration file
--------------------------------------------------------------

Passing the URI to a lazy connection can be annoying, since URIs
are quite verbose. This problem is solved by the builtin aliasing mechanism
of ``sqlplain``. The trick is the following: as argument of a lazy connection
you can use a true uri, i.e. anything starting with ``mssql://`` or
``postgres://`` or ``sqlite://`` or also an alias, i.e. a plain Python name
(I recommend to use lowercase letters, digits and underscores only,
even if this is not forced). The alias is interpreted by looking
at the ``sqlplain`` configuration file.

The location of the configuration file is determined by the environment
variable ``$SQLPLAIN``: if empty or missing, the configuration file is
assumed to be located in ``~/.sqlplain`` where ``~`` means the current
user home directory. The configuration file has a ``[uri]`` section
containing the expansion of the aliases, i.e. the mapping from the alias
name to the full URI. For instance, this could be an example of a valid
configuration file::

 $ echo ~/.sqlplain
 [uri]
 bookdb: mssql://pyadmin:secret@localhost/bookdb
 testdb: sqlite:///:memory:

The configuration file is read when the lazy connection is instantiated:
if an alias is found, the corresponding true
uri is parsed and the correct database driver is loaded, otherwise a
``NameError``
is raised. If the configuration file is missing, an ``ImportError`` is raised.

Lazy connections can be used as global variables
(do not believe people saying that globals are evil: Python is full of
globals, modules are global variables, classes are global variables, and
there is nothing wrong in having lazy connection as globals). If
you instantiate your lazy connections at the beginning
of your module, then the underlying low level database driver
is imported when your module is imported. If you follow this pattern,
then, the configuration file is part of your application and
you should consider it as required Python code, even if for sake of simplicity
it uses the traditional .INI format. If you distribute code based on sqlplain,
the user is supposed to edit the configuration file by setting the correct
database uri for her database. Of course, you can always write a small
application to set the configuration file if you don't want your users
to touch the .INI file directly (you could set it at installation time,
or write a small GUI to edit the configuration file).

A typical way to pass the URI is to read it from the command line:

.. code-block:: python

 $ cat example_sqlplain_app.py
 import sqlplain

 def main(db):
     # do something with the lazy connection db
    
 if __name__ == '__main__':
    main(sys.argv[1]) # add argument parsing at will

This works if ``sys.argv[1]`` is a valid URI or a valid alias.
However, if you are writing functional tests and you invoke them
with (say) nose, you cannot use this pattern since ``sys.argv[1]``
is the test file. When writing nose tests it makes sense to
use a global lazy connection, instantiated at the top of your
testing script, something like ``testdb = lazyconnect('testdb')`` where
``testdb`` is an alias to the database used for your automatic tests.

Transactions
--------------------------------------------------------------

By default ``sqlplain`` works in autocommit mode. If you want to use
transactions, you must specify the isolation level. When running
in transactional mode, your lazy connection is an instance of
``TransactionalConnection``, a subclass of ``LazyConnection``
with methods ``commit`` and ``rollback``

.. code-block:: python

 >> bookdb = lazyconnect('mssql://pyadmin:secret@localhost/bookdb',
                         isolation_level='SERIALIZABLE')

Transactional connections have support the ``with`` statement,
therefore if you are using a recent enough version of Python (2.5+) you can
write

.. code-block:: python

  with booksb: # will begin a transaction and commit or rollback it
      do_something
      
Otherwise, ``sqlplain`` provides a ``transact`` higher order function coding
the ``rollback/commit`` pattern:

$$transact

There is also a ``dry_run`` functionality to try out a procedure
non-destructively:

$$dry_run
      
Threadlocal connections
-------------------------------------------------------

The typical user of ``sqlplain`` is expected to write simple programs,
for instance scripts for extracting statistics from a database, or
import scripts, or maintenance scripts. The idea is to provide a simple
library to perform simple tasks. User wanted something more sophisticated
(for instance people writing a server program connected to a database
and managing many users) are expected to use a more sophisticated
database toolkit. This is the reason why ``sqlplain`` does not provide
a connection pool and it will never provide one. You are supposed to
use the connection pool of your database driver (for instance psycopg2
provides one), the connection pool of SQLAlchemy, or a custom made one.
Having said so, ``sqlplain`` provides some support for server programs.
The reason is that I often need to wrap a script with a Web interface,
and at work we use Pylons as web framework. Pylons comes with the Paste
Web server which is fine for usage in our local intranet. The Paste
server is a multithreaded server which internally manages a pool of threads.
To make ``sqlplain`` to work in this situation, you must set the threadlocal
flag: doing so ensure that each thread gets its own lower level
connection, independent from the connections of the other threads. 

 >> conn = lazyconnect('sqlite:///:memory:', threadlocal=True)

Here in an example script showing multiple threads writing on a sqlite
database; if you forget to set the ``threadlocal`` flag, you will likely
incur in errors (for instance I get ``OperationalError: database is locked``).

$$threadlocal_ex

I have been using this approach for one year in production on linux
without problems, however, only in situations of low concurrency
and only in autocommit mode. You should consider the multithread
support of ``sqlplain`` as experimental and there is no guarantee
it will work in your settings. Also, the multithreading support is
very low in my list of priorities (I am in the camp of people who
are against thread) and what it is there is the minimun I needed
to do in order make my scripts work with the Paste server.

SQL template functions
--------------------------------------------------------------

``sqlplain`` allows you to litter you source code with scattered
SQL queries, but also offers you the possibility to collect
your queries in a common place. For instance, you could write
all of your SQL templates in a file called ``queries.py``
like the following

$$queries

The ``sqlplain.do`` utility converts a SQL template into a Python
function with signature ``(conn, arg1, ...,  argN)`` where ``conn``
is a ``sqlplain`` connection and arg1, ..., argN are arguments
in correspondence with the question marks in the template.
Moreover, the docstring of the generated functions is set to the
SQL template, unless you specify a different docstring. That
means that the built-in ``help`` functionality (as well as
documentation tools) play well with the generated functions.
Here are a few examples:

.. code-block:: python

 >> from queries import *
 >> help(get_authors)
 Help on function sqlquery in module queries:

 sqlquery(conn)
     SELECT author FROM book
 Help on function sqlquery in module queries:
 
 >> help(get_titles)
 sqlquery(conn, arg1)
     SELECT title FROM book WHERE author=?
 
 >> help(set_uppercase_titles)
 Help on function sqlquery in module queries:
 
 sqlquery(conn, arg1, arg2)
     UPDATE book SET title=upper(title)
     WHERE author like ? AND pubdate=?

By default all the functions generated by ``do`` have the name
``sqlquery``, but is possible to specify a different name; it
is even possible to specify the names of the arguments. For
instance, we could have defined ``set_uppercase_titles`` as
follows:

.. code-block:: python

 >> set_uppercase_titles = do('''
 UPDATE book SET title=upper(title)
 WHERE author like ? AND pubdate=?
 ''', name='set_uppercase_titles', args='author, pubdate')

 >> help(set_uppercase_titles)
 Help on function set_uppercase_titles in module queries:

 set_uppercase_titles(conn, author, pubdate)
     UPDATE book SET title=upper(title)
     WHERE author like ? AND pubdate=?

Setting the function name and the argument names explicitly is a good idea
if you want to have readable error messages in case of errors.

Memoization
-------------------------------------------------------------

In my day job often I have to work with heavy queries
which results must be cached. Since this is a common requirement,
I have decided to provide a simple caching facility in ``sqlplain``.
The core functionality is provided by the ``Memoize`` class
in ``sqlplain.memoize``. ``Memoize`` takes in input a new-style class
(the cache type) and returns a decorator object which is able to
memoize functions. The cache type is attached as an attribute
to the memoized function. Moreover any memoized function has
a .cache attribute (a dictionary <function args> -> <function result>)
which is looked up when the function is called a second time.
If the second time the function is called with the same arguments
the result is retrieved from the cache.

A global registry of the memoized
functions is kept in memory and there is a classmethod
``Memoize.clear(cachetype=object)`` which is able to clear the cache.
If you specify the cachetype, only the functions with a cache type
which is a subclass of the specified one will be affected.
If you do not specify the cachetype, by default ``object`` will be
assumed, therefore *all* caches for all memoized functions will be
cleared.

Here is an example of use:

$$cache_ex

Here the cache of ``f1`` is cleared, but the cache of
``f2`` is not cleared.

According to the goal of keeping things simple, ``sqlplain``
does not provide the concept of cache expiration, and you are
expected to clear the cache by hand. Anyway, it is pretty easy to schedule
a cache cleaner function to be run periodically (which of course depends on
the framework you are using) and you can implement it yourself.

``sqlplain`` tries to make your life easier when you are interested
in caching simple queries: to this goal, the ``do`` utilities has a
``cachetype`` default argument which you can set to enable caching::

 >> get_title = do('select title from book where author=?', cachetype=ShortType)

Internally ``do`` generates a Python function from the SQL template and
memoize it by using ``Memoize(cachetype)``; this is the reason why
the resulting function has a ``.cachetype`` attribute::

 >> get_title.cachetype
 <class __main__.ShortType at 0x9ebc32c>


Utilities
--------------------------------------------------------------

``sqlplain`` is a very poor toolkit compared to other database toolkits;
this is done on purpose. Nevertheless, it provides a few convenient
functions to work with a database directly, collected in the ``util``
module. They are the following::

    openclose(uri, templ, *args, **kw):

    exists_db drop_db create_db(uri, drop=False),
    make_db(alias=None, uri=None, dir=None):


Moreover, there are a few utilities to manage database schemas, which
are a PostgreSQL-only feature: ``set_schema(db, name), exists_schema(db, name),
drop_schema(db, name), create_schema(db, schema, drop=False), make_schema``.

``sqlplain`` provide some limited introspection features (the introspection
features are likely to be enhanced in future versions). For the moment,
the only things you can do is to introspect a table or a view and to
return a named tuple with the names of the fields:

    

An example project using sqlplain: books
--------------------------------------------------

In this section I discuss a toy project realized with sqlplain, i.e.
an archive of the books I have read. I did start keeping track of
the books I read more than twenty years ago, and writing a program
to make statistics about my books was one of my first programs ever.
It is nice to come back to the same problem after twenty years, now that I
know SQL ;)
I will implement the project by using a test first approach.

"""

from sqlplain.doc import threadlocal_ex
from sqlplain import transact, dry_run
import queries, cache_ex
