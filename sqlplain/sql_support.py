import re, inspect
from decorator import FunctionMaker

STRING_OR_COMMENT = re.compile(r"('[^']*'|--.*\n)")

templ_cache = {}

# used in .execute
def qmark2pyformat(templ):
    # this is small hack instead of a full featured SQL parser
    """
    Take a SQL template and replace question marks with pyformat-style
    placeholders (%s), except in strings and comments. Return the number
    of replaced qmarks and the new template. The results are cached.
    """
    if templ in templ_cache:
        return templ_cache[templ]
    qmarks = 0
    out = []
    for i, chunk in enumerate(STRING_OR_COMMENT.split(templ)):
        if i % 2 == 0: # real sql code
            qmarks += chunk.count('?')
            out.append(chunk.replace('?', '%s'))
        else: # string or comment
            out.append(chunk)
    new_templ = ''.join(out)
    templ_cache[templ] = qmarks, new_templ
    return qmarks, new_templ

# used in 'do' queries
def extract_argnames(templ):
    '''
    Given a template with question marks placeholders, returns
    a list of arguments of the form ['arg1', ..., 'argN'] where
    N is the number of question marks.
    '''
    qmarks = 0
    for i, chunk in enumerate(STRING_OR_COMMENT.split(templ)):
        if i % 2 == 0: # real sql code
            qmarks += chunk.count('?')
    return ['arg%d' % i for i in range(1, qmarks + 1)]
            
def do(templ, name='sqlquery', argnames=None, defaults=None, scalar=False,
       ntuple=None):
    """
    Compile a SQL query template down to a Python function with attributes
    __source__, argnames defaults, scalar, ntuple. argnames is a comma
    separated string of names, whereas defaults is a tuple.
    """
    if argnames is None:
        argnames = ', '.join(extract_argnames(templ))
        if argnames:
            argnames += ','
    src = '''def %(name)s(conn, %(argnames)s):
    return conn.execute(templ, (%(argnames)s), scalar=scalar, ntuple=ntuple)
    ''' % locals()
    fn = FunctionMaker(
        name=name, signature=argnames, defaults=defaults, doc=templ).make(
        src, dict(templ=templ, scalar=scalar), addsource=True)
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
    
