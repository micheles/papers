"""
Trivial example of using MySQLdb from twisted.
"""
from twisted.internet import reactor
from twisted.enterprise import adbapi
from ms.debug_utils import printf

def prnt(ls):
    for el in ls:
        print el

if __name__ == "__main__":
    dbpool = adbapi.ConnectionPool("MySQLdb", db='test')
    dbpool.runQuery("select * from books where genre='FS'").addCallbacks(
        prnt, printf)
    reactor.run()
