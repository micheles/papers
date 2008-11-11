"""
usage: %prog [options]
-r, --rst: make an .rst file only
"""

import os, sys, re, webbrowser
from docutils.core import publish_cmdline
from ms.optionparser import OptionParser

BIGCOMMENT = re.compile(r'#\|(.*)\|#(.*)', re.DOTALL)
SNIPPET = re.compile(r'\n;\s*([-A-Z\d_/!\?]+)\s*\n(.*?)\n\s*;END', re.DOTALL)
SNIPPETNAME = re.compile(r'\n\$\$([-A-Z\d_/!\?]+)\n')
INCLUDE = re.compile(r'@@([-\w\d_\.]+)')

def indent(text):
    return '\n'.join('  ' + ln for ln in text.splitlines())

def scheme2rst(fname, codeblock=False):
    mo = BIGCOMMENT.search(file(fname).read())
    if mo is None: 
        sys.exit('No #| ..|# found!')
    text, code = mo.groups()
    snippet = dict((mo.group(1), mo.group(2)) for mo in SNIPPET.finditer(code))
    if codeblock:
        templ = '\n.. code-block:: scheme\n\n%s\n'
    else:
        templ = '\n::\n\n%s\n'
    def repl(mo):
        "replace SNIPPETNAME with the content of the snippet dictionary"
        name = mo.group(1)
        return templ % indent(snippet[name])
    def include_file(mo):
        return templ % indent(file(mo.group(1)).read())
    rst = INCLUDE.sub(include_file, SNIPPETNAME.sub(repl, text))
    rstfile = os.path.splitext(fname)[0] + '.rst'
    file(rstfile, 'w').write(rst)
    return rstfile

if __name__ == "__main__":
    option, schemefiles = OptionParser(__doc__).parse_args()
    if not option and not schemefiles:
        OptionParser.exit("usage: python rst2st.py <scheme source file>")
    for schemefile in schemefiles:
        fname, ext = os.path.splitext(schemefile)
        assert ext in ('.ss', '.scm')
        rstfile = scheme2rst(schemefile) # generate .rst
        if not option.rst: # generate .html unless option -r was set
            htmlfile = fname + '.html'
            publish_cmdline(writer_name='html', argv=[rstfile, htmlfile])
            webbrowser.open(htmlfile)
