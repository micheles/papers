import os, sys, shutil
from scheme2rst import SNIPPET

ikarus_code = file('sweet-macros/main.sls').read()

# GUARDED-SYNTAX-CASE, SYNTAX-MATCH, DEF-SYNTAX, SYNTAX-EXPAND
snippets = [s.groups() for s in SNIPPET.finditer(ikarus_code)]
snippet = dict(snippets)

helper1 = '''#!r6rs
(library (sweet-macros helper1)
(export guarded-syntax-case)
(import (rnrs))

%(GUARDED-SYNTAX-CASE)s
)
'''

helper2 = '''#!r6rs
(library (sweet-macros helper2)
(export syntax-match)
(import (rnrs) (for (rnrs) (meta -1))
(for (sweet-macros helper1) (meta -1) (meta 0) (meta 1)))

%(SYNTAX-MATCH)s
)
'''

helper3 = '''#!r6rs
(library (sweet-macros helper3)
(export syntax-match def-syntax)
(import (rnrs) (for (sweet-macros helper2) run expand))

%(DEF-SYNTAX)s
)
'''

main = '''#!r6rs
(library (sweet-macros)
(export syntax-match def-syntax syntax-expand)
(import (rnrs) (for (sweet-macros helper3) run expand))

%(SYNTAX-EXPAND)s
)
'''

def write_on(fname, code):
    file(fname, 'w').write(code)
    os.system('zip sweet-macros %s' % fname)
    
def makefiles(name, snippet):
    write_on(name + '/main.sls', ikarus_code)
    write_on(name + '/main.mzscheme.sls', main % snippet)
    write_on('sweet-macros.larceny.sls', main % snippet)
    write_on('sweet-macros.mosh.sls', ikarus_code)
    write_on(name + '/helper1.sls', helper1 % snippet)
    write_on(name + '/helper2.sls', helper2 % snippet)
    write_on(name + '/helper3.sls', helper3 % snippet)
    
if __name__ == '__main__':
    #plt_home = os.path.expanduser('~/.plt-scheme')
    #collects = os.path.join(plt_home, max(os.listdir(plt_home)), 'collects')
    makefiles('sweet-macros', snippet)
    os.system('scp sweet-macros.zip '
              'micheles@merlin.phyast.pitt.edu:public_html/scheme')
