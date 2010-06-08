"""                                    
The table framework
------------------------------------------------------------

As I said in the introduction, ``sqlplain`` is not intended to be
a fully fledged ORM, therefore it does not provide a builtin mechanism
to map user defined classes to database objects, such as the mapping
mechanism in SQLAlchemy, or the popular Active Record pattern; nevertheless,
it provides a table framework which us a lightweight object oriented layer
over database tables.
``sqlplain`` table object comes in two flavors: D-tables, which should be
used for tables without a primary key, and K-tables, which must
used for tables with a primary key, possibly composite. Actually K-tables
are D-tables too, since they inherit from D-tables, therefore they
share all D-tables methods and they add a few methods which are
meaninful only if the underlying table has a primary key.
If you try to use a K-table over a database table which has no primary
key, a ``TypeError`` will be raised. Here is an example:

$$logtable

>>> from sqlplain import table
>>> import logtable
>>> db = logtable.init('sqlite_test')
>>> log = table.KTable(db, 'log')
Traceback (most recent call last):
  ...
TypeError: table log has no primary key!

Using a DTable instead works:

>>> log = table.DTable.reflect(db, 'log')

The ``.reflect`` classmethod creates a suitable subclass of ``DTable``
(called ``Log``) and instantiates it. ``DTable`` is a kind of abstract
base class and you cannot instantiate it directly (the same is true
for the KTable class):

>>> table.DTable(db)
Traceback (most recent call last):
  ...
TypeError: You cannot instantiate the ABC DTable

In order to create a concrete subclass of DTable (or KTable) one needs
to set the tabletuple class attribute ``tt``, which contains information
about the table structure. The ``.reflect`` method extracts the information
from the database schema; for instance ``log.tt`` is a namedtuple with
fields ``date`` and ``message``:

>>> print log.tt._fields
('date', 'message')


>>> from datetime import datetime
>>> now = datetime.now
>>> log.insert_row(date=now(), message='message1')
1
>>> log.insert_row(date=now(), message='message2')
1
>>> print len(log)
2

Here is the full API for DTable:

- create
- select
- count
- delete
- truncate
- insert_row
- insert_rows
- load_file

Here are the additional functions of K-tables:
    
- select_row
- update_row
- update_or_insert_row
- delete_row

Instead of an explanation, I will give examples::

    select_book = select_row('book', 'title author')
    select_book(bookdb, title='T', author='A')

``select_row`` raises an error if the corresponding queries
returns no result (you are looking for a missing record) or
if it returns multiple results (it means that your primary key specification
was incomplete).
"""
