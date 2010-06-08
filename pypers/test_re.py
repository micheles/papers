"""This script looks at its own source code and extracts dotted names,
i.e. names containing at least one dot, such as object.attribute or
more general one, such as obj.attr.subattr."""
# Notice that dotted.names in comments and literal strings are ignored
if __name__ == "__main__":
    from oopp import *
    import __main__
    text = inspect.getsource(__main__)
    regexp = Regexp.CODESEP| Regexp.DOTNAME()
    print 'Using the regular expression',regexp
    print "I have found the following dotted names:\n%s" % [
        MO.group() for MO in regexp.finditer(text) if MO.lastgroup=='DOTNAME']

