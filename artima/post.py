"""
A script to post articles on my blog
"""

import re, sys
from twill import commands as c

IMAGE = re.compile('e:: ([-+\w\.]+)') # figure/image

def replace_image_links(txt):
    "Remove code-block directives and relative images to make Artima happy"
    txt = re.sub(r'.. code-block:: \w+', '::', txt)
    return IMAGE.sub(r'e:: http://www.phyast.pitt.edu/~micheles/scheme/\1', txt)

if __name__ == '__main__':
    try:
        rstfile, thread  = sys.argv[1:]
    except ValueError:
        sys.exit('Usage: post <rstfile> <artima-thread-number>')
    text = replace_image_links(file(rstfile).read())
    c.go('http://www.artima.com/sign_in?d=%2Findex.jsp')
    c.formvalue('1', 'username', 'micheles')
    c.formvalue('1', 'password', 'pippolippo')
    c.submit()
    c.go('http://www.artima.com/weblogs/editpost.jsp?thread=%s' % thread)
    try:
        c.formvalue('1', 'body', text)
    except:
        print "Saving the offending post on /tmp/x.txt ..."
        file('/tmp/x.txt', 'w').write(text)
        raise
    c.submit()

"""
from ms.http_utils import urlopen2
urlopen2('http://www.artima.com/sign_in?d=%2Findex.jsp', 
  dict(username='micheles', password='pippolippo'))
urlopen2('http://www.artima.com/weblogs/editpost.jsp',
  dict(thread='261364', body='prova'))
"""
