"""
A script to post articles on my blog
"""

import sys
from twill import commands as c

if __name__ == '__main__':
    try:
        rstfile, thread  = sys.argv[1:]
    except ValueError:
        sys.exit('Usage: post <rstfile> <artima-thread-number>')
    text = file(rstfile).read()
    c.go('http://www.artima.com/sign_in?d=%2Findex.jsp')
    c.formvalue('1', 'username', 'micheles')
    c.formvalue('1', 'password', 'pippolippo')
    c.submit()
    c.go('http://www.artima.com/weblogs/editpost.jsp?thread=%s' % thread)
    c.formvalue('1', 'body', text)
    c.submit()
