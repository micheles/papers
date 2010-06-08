#!/usr/bin/env python
"""$URL: svn+ssh://svn.mems-exchange.org/repos/trunk/quixote/server/fastcgi_server.py $
$Id: fastcgi_server.py 25476 2004-10-27 21:54:59Z nascheme $

Server for Quixote applications that use FastCGI.  It should work
for CGI too but the cgi_server module is preferred as it is more
portable.
"""

from quixote.server import _fcgi
from quixote.http_request import HTTPRequest

def run(create_publisher):
    publisher = create_publisher()
    while _fcgi.isFCGI():
        f = _fcgi.FCGI()
        request = HTTPRequest(f.inp, f.env)
        response = publisher.process_request(request)
        try:
            response.write(f.out)
        except IOError, err:
            publisher.log("IOError while sending response ignored: %s" % err)
        f.Finish()


if __name__ == '__main__':
    from quixote.demo import create_publisher
    run(create_publisher)
