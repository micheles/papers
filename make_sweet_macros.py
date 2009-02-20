import os, sys, shutil
from scheme2rst import SNIPPET

code = file('scheme/sweet-macros.sls').read()

# LOCAL, GUARDED-SYNTAX-CASE, SYNTAX-MATCH, DEF-SYNTAX, SYNTAX-EXPAND
snippets = [s.groups() for s in SNIPPET.finditer(code)]
snippet = dict(snippets)

helper1 = '''#!r6rs
(library (sweet-macros helper1)
(export local guarded-syntax-case)
(import (rnrs))

%(LOCAL)s

%(GUARDED-SYNTAX-CASE)s
)
'''

helper2 = '''#!r6rs
(library (sweet-macros helper2)
(export local guarded-syntax-case syntax-match)
(import (rnrs) (for (sweet-macros helper1) run expand))

%(SYNTAX-MATCH)s
)
'''

main = '''#!r6rs
(library (sweet-macros)
(export local guarded-syntax-case syntax-match def-syntax syntax-expand)
(import (rnrs) (for (sweet-macros helper2) run expand))

%(DEF-SYNTAX)s

%(SYNTAX-EXPAND)s
)
'''

def makedir(name, snippet):
    try:
        shutil.rmtree(name)
    except OSError:
        pass # already removed
    os.mkdir(name)
    file(name + '/helper1.sls', 'w').write(helper1 % snippet)
    file(name + '/helper2.sls', 'w').write(helper2 % snippet)
    file(name + '/main.sls', 'w').write(main % snippet)
    
if __name__ == '__main__':
    plt_home = os.path.expanduser('~/.plt-scheme')
    collects = os.path.join(plt_home, max(os.listdir(plt_home)), 'collects')
    os.chdir(collects)
    makedir('sweet-macros', snippet)
    os.system('zip -r /tmp/sweet-macros-plt sweet-macros')
    print 'Saved in /tmp/sweet-macros-plt'
