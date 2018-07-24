# bug.py

"""

Test a bug of doctest in redirecting stdout.

The module prnt is as follows:

# prnt.py
import sys
f=sys.stdout

def hello():
    print >> f, 'hello'

Notice that

  print >> sys.stdout, 'hello'

and

  print 'hello'
  
would work instead!

"""

import doctest,__main__

def f1():
    """
    First docstring saying

    >>> import prnt
    >>> prnt.hello()
    hello
    
    """
    
def f2():
    """
    Second docstring saying
    
    >>> import prnt
    >>> prnt.hello()
    hello
    
    """
    
doctest.testmod(__main__)
