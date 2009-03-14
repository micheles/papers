import os, sys, shutil
from scheme2rst import SNIPPET

code = file('sweet-macros/main.sls').read()

# GUARDED-SYNTAX-CASE, SYNTAX-MATCH, DEF-SYNTAX, SYNTAX-EXPAND
snippets = [s.groups() for s in SNIPPET.finditer(code)]
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
(library (sweet-macros)
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

def makefiles(name, snippet):
    file(name + '/helper1.mzscheme.sls', 'w').write(helper1 % snippet)
    file(name + '/helper2.mzscheme.sls', 'w').write(helper2 % snippet)
    file(name + '/helper3.mzscheme.sls', 'w').write(helper3 % snippet)
    file(name + '/main.mzscheme.sls', 'w').write(main % snippet)
    #os.system('zip -r %s %s' % (name, name))
    
if __name__ == '__main__':
    #plt_home = os.path.expanduser('~/.plt-scheme')
    #collects = os.path.join(plt_home, max(os.listdir(plt_home)), 'collects')
    makefiles('sweet-macros', snippet)
  
