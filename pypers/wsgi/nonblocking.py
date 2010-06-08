from __future__ import with_statement
import time, threading
from easy_async import make_reactor
from wsgiref.simple_server import make_server
from ms.debug_utils import printdict
from paste.auth.basic import AuthBasicHandler

## def blockingcalc(env, resp):
##     resp('200 OK', [('Content-type', 'text/html')])
##     res = 0
##     for i in range(1000):
##         time.sleep(.01)
##         res += i
##     printdict(env)
##     return ['The result is %d' % res]

def _nonblockingcalc(job):
    res = 0
    for i in range(1000):
        time.sleep(.01)
        res += i
        yield 'Partial result %d' % res
    yield 'The result is %d' % res

_cache = {}

def next(gen, env):
    user = env['REMOTE_USER'] # not null because of the middleware
    job = _cache.get(user)
    if job is None: # create a new job
        job = _cache[user] = reactor.Job(gen)
        job.tick = 0
        job.start()
    return job.yval

def nonblockingcalc(env, resp):
    resp('200 OK', [('Content-type', 'text/html')])
    yield str(next(_nonblockingcalc, env))
    
if __name__ == '__main__':
    app = AuthBasicHandler(
        nonblockingcalc, 'realm', lambda e, u, p: u=='pippo')
    reactor = make_reactor('default')
    job = reactor.Job(_nonblockingcalc)
    job.start()
    
    with reactor.in_separated_thread():
        make_server('', 8000, app).serve_forever()
