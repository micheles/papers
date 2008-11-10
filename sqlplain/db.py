import sys
from operator import itemgetter
try:
    from collections import namedtuple
except ImportError:
    from sqlplain.namedtuple import namedtuple

from sqlplain.dissect import get_connect_params

class TupleList(list):
    "Used as result of execute"
    header = None
    rowcount = None
    
class DB(object):
    """
    A lazy callable object returning recordsets. It is lazy since the
    database connection is performed at calling time, not at inizialization
    time.  The connection factory must return (memoized) connections.
    DBI objects can also be used to perform
    actions on the database, via the 'execute' method.
    Notice that this class does not manage any kind of logging, on purpose.
    There easy however a chatty method for easy of debugging.
    """
    @classmethod
    def connect(cls, alias, autocommit=True):
        return cls(configurator.get_uri(alias))
                   
    def __init__(self, uri, autocommit=True, threadlocal=False):
        self.connect, params = get_connect_params(uri)
        self.args = params, autocommit
        self._conn = None
        self.chatty = False
        self.uri = uri
        self.autocommit = autocommit
        self.threadlocal = threadlocal
        if self.threadlocal:
            self.loc = threading.local()
        else:
            self.loc = self
        if not self.autocommit:
            def rollback(self): return self.conn.rollback()
            def commit(self): return self.conn.commit()
            self.rollback = rollback.__get__(self)
            self.commit = commit.__get__(self)

    def _raw_execute(self, templ, args, ntuple):
        cursor = self.curs
        if self.chatty:
            print cursor.rowcount, templ, args
        if args:
            cursor.execute(templ, args)
        else:
            cursor.execute(templ)
        if cursor.description is None: # after an update
            return cursor.rowcount
        else: # after a select
            rows = cursor.fetchall()
            fields = map(itemgetter(0), cursor.description)
            Ntuple = ntuple or namedtuple('DBTuple', fields)
            res = TupleList(Ntuple(*row) for row in rows)
            res.header = Ntuple(*fields)
            return res

    def _safe_execute(self, templ, args, ntuple):
        "Connection with automatic rollback if transactional"
        try:
            return self._raw_execute(templ, args, ntuple)
        except Exception, e:
            if not self.autocommit:
                self.rollback()
            raise e.__class__, e, sys.exc_info()[2]
        
    ## execute a query retrying it once when losing connection
    def execute(self, templ, args=(), ntuple=None, firstcall=True):
        try:
            return self._raw_execute(templ, args, ntuple)
        except Exception, e:
            #import pdb; pdb.set_trace()
            errname = e.__class__.__name__
            if errname == 'OperationalError' and firstcall:
                # hack to make this portable for many drivers
                # print errname, e, 'reconnecting to database ...'
                self.reset_conn()
                return self._raw_execute(templ, args, ntuple, firstcall=False)
            else:
                tb = sys.exc_info()[2]
                raise e.__class__, '%s\nQUERY WAS:%s%s' % (e, templ, args), tb
    
    def reset_conn(self):
        """The next time you will call an active method, a fresh new
        connection will be instantiated"""
        loc = self.loc
        loc._cursor.close()
        loc._connection.close()
        try:
            delattr(loc, '_connection')
            delattr(loc, '_cursor')
        except AttributeError:
            pass # conn and curs not instantiated yet, ignore

    def __repr__(self):
        return "<DB %s, autocommit=%s>" % (self.uri, self.autocommit)

    @property
    def conn(self):
        loc = self.loc
        try:
            conn_ = loc._connection
        except AttributeError:
            conn_ = loc._connection = self.connect(*self.args)
        return conn_
   
    @property
    def curs(self):
        loc = self.loc
        try:
            curs_ = loc._cursor
        except AttributeError:
            curs_ = loc._cursor = self.conn.cursor()
        return curs_
