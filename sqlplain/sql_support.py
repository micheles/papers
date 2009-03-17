import re, inspect
from decorator import FunctionMaker

class _SymbolReplacer(object):
    """
    A small internal class to parse SQL templates with names arguments.
    Returns the names of the arguments and the template interpolated with
    a placeholder. Used by get_args_templ.
    """
    STRING_OR_COMMENT = re.compile(r"('[^']*'|--.*\n)")
    SYMBOL = re.compile(r":(\w+)")
    
    def __init__(self, placeholder):
        self.placeholder = placeholder
        self.replaced = []
        self.found = set()

    def get_args_templ(self, templ):
        argnames = []
        def repl(mo):
            argname = mo.group(1)
            if argname in argnames:
                raise NameError('Duplicate argument %r in SQL template'
                                % argname)
                argnames.append(argname)
            return self.placeholder or mo.group()
        out = []
        for i, chunk in enumerate(self.STRING_OR_COMMENT.split(templ)):
            if i % 2 == 0: # real sql code
                chunk = self.SYMBOL.sub(repl, chunk)
            out.append(chunk)
        return argnames, ''.join(out)

templ_cache = {}

# used in .execute
def get_args_templ(templ, repl=None):
    # this is small hack instead of a full featured SQL parser
    """
    Take a SQL template and replace named arguments with the repl, except
    in strings and comments. Return the replaced arguments and the new
    template. The results are cached.

    >>> args, templ = get_args_templ('INSERT INTO book (:title, :author)')
    >>> print args
    ['title', 'author']
    >>> print templ
    INSERT INTO book (:title, :author)
    >>> print get_args_templ('INSERT INTO book (:title, :author)', '?')[1]
    INSERT INTO book (?, ?)
    """
    if (templ, repl) in templ_cache:
        return templ_cache[templ, repl]
    argnames, new_templ = _SymbolReplacer(repl).get_args_templ(templ)
    templ_cache[templ, repl] = argnames, new_templ
    return argnames, new_templ
            
def do(templ, name='sqlquery', defaults=None, scalar=False, ntuple=None):
    """
    Compile a SQL query template down to a Python function with attributes
    __source__, argnames defaults, scalar, ntuple. defaults is a tuple.
    """
    argnames = ', '.join(get_args_templ(templ)[0])
    if argnames:
        argnames += ','
    src = '''def %(name)s(conn, %(argnames)s):
    return conn.execute(templ, (%(argnames)s), scalar=scalar, ntuple=ntuple)
    ''' % locals()
    fn = FunctionMaker(
        name=name, signature=argnames, defaults=defaults, doc=templ).make(
        src, dict(templ=templ, scalar=scalar, ntuple=ntuple), addsource=True)
    comment = '# ntuple = %s\n# scalar = %s\n# templ=\n%s\n' % (
        ntuple, scalar, '\n'.join('## ' + ln for ln in templ.splitlines()))
    fn.__source__ = '%s\n%s' % (comment, fn.__source__)
    fn.templ = templ
    fn.argnames = argnames
    fn.defaults = defaults
    fn.scalar = scalar
    fn.ntuple = ntuple
    return fn

def spec(fn, clause, argnames=None, defaults=None, ntuple=None):
    "Add a clause to an SQL Template function"
    return do(fn.templ + clause, argnames = argnames or fn.argnames,
              defaults=defaults or fn.defaults, 
              scalar=fn.scalar, ntuple=ntuple or fn.ntuple)
    

if __name__ == '__main__':
    import doctest; doctest.testmod()
