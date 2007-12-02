"""This script looks at its own source code and extracts dotted names,
i.e. names containing at least one dot, such as object.attribute or
more general one, such as obj.attr.subattr."""

# Notice that dotted.names in comments and literal strings are ignored

from oopp import *
import __main__

text=inspect.getsource(__main__)

dotname=Regexp.CODESEP| Regexp.DOTNAME.named()

print 'Using the regular expression',dotname

print "I have found the following dotted names:\n%s" % [
    MO.group() for MO in dotname.finditer(text)]
    
