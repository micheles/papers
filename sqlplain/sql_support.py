import re, sys
from sqlplain.python import makefn, FuncData
from sqlplain.memoize import Memoize

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
    templ_cache[templ] = qmarks, templ
    return qmarks, ''.join(out)

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
            
def do(templ, name='sqlquery', args=None, defaults=None, doc=None,
       getone=False, cachetype=None):
    """
    Compile a SQL query template down to a Python function.
    """
    if args is None:
        args = ', '.join(extract_argnames(templ))
    if args:
        args += ','
    src = '''def %(name)s(conn, %(args)s):
    return conn.execute(templ, %(args)s getone=getone)''' % locals()
    fd = FuncData(name=name, signature=args, defaults=defaults,
                  doc=doc or templ,
                  module=sys._getframe(1).f_globals['__name__'])
    func = makefn(src, fd, templ=templ, getone=getone)
    if cachetype:
        return Memoize(cachetype)(func)
    else:
        return func
