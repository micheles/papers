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
            
def do(templ, name='sqlquery', args=None, defaults=None, scalar=False):
    """
    Compile a SQL query template down to a Python function.
    """
    if args is None:
        args = ', '.join(extract_argnames(templ))
    if args:
        args += ','
    src = '''def %(name)s(conn, %(args)s):
    return conn.execute(templ, %(args)s scalar=scalar)''' % locals()
    fun = FunctionMaker(name=name, signature=args, defaults=defaults,
                        doc=templ)
    fn = fun.make(src, dict(templ=templ, scalar=scalar), addsource=True,
                    templ=templ)
    comment = '# scalar = %s\n# templ=\n%s\n' % (scalar, '\n'.join(
        '## ' + line for line in templ.splitlines()))
    fn.__source__ = '%s\n%s' % (comment, fn.__source__)
    return fn
