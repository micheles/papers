#!/usr/bin/env python
"""$URL: svn+ssh://svn.mems-exchange.org/repos/trunk/quixote/server/cgi_server.py $
$Id: cgi_server.py 25476 2004-10-27 21:54:59Z nascheme $
"""

import sys
import os
from quixote.http_request import HTTPRequest

def run(create_publisher):
    if sys.platform == "win32":
        # on Windows, stdin and stdout are in text mode by default
        import msvcrt
        msvcrt.setmode(sys.__stdin__.fileno(), os.O_BINARY)
        msvcrt.setmode(sys.__stdout__.fileno(), os.O_BINARY)
    publisher = create_publisher()
    request = HTTPRequest(sys.__stdin__, os.environ)
    response = publisher.process_request(request)
    try:
        response.write(sys.__stdout__)
    except IOError, err:
        publisher.log("IOError while sending response ignored: %s" % err)


if __name__ == '__main__':
    from quixote.demo import create_publisher
    run(create_publisher)
