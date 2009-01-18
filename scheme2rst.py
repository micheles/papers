"""
usage: %prog [options]
-r, --rst: make an .rst file only
"""

"""
This script is able to include libraries in the IKARUS_LIBRARY_PATH
(with the @@NAME) syntax and to include snippets defined in the
current file.
"""

import os, sys, re, webbrowser
from docutils.core import publish_cmdline
from ms.optionparser import OptionParser

BIGCOMMENT = re.compile(r'#\|(.*)\|#(.*)', re.DOTALL)
SNIPPET = re.compile(r'\n;+\s*([-A-Z\d_/!\?]+)\s*\n(.*?)\n\s*;+\s*END',
                     re.DOTALL)
SNIPPETNAME = re.compile(r'\n\$\$([-A-Z\d_/!\?]+)\n')
INCLUDE = re.compile(r'\$\$([-\w\d_\.]+):')
INCLUDESNIPPET = re.compile(r'\$\$([-\w\d_\.]+):([-A-Z\d_/!\?]+)\n')

PATH = os.environ['IKARUS_LIBRARY_PATH']

def include(fname, paths=('.', PATH), exts=('.ss', '.sls')):
    for path in paths:
        for ext in exts:
            try:
                return file(os.path.join(path, fname + ext)).read()
            except IOError, e:
                pass
    raise e

def indent(text):
    return '\n'.join('  ' + ln for ln in text.splitlines())

class SnippetExtractor(object):
    """
    Given some Scheme code, parses it to find snippets. Provides a .get method
    to extract a properly indented snippet and a cached .make classmethod to
    extract the code from a file name.
    """
    _cache = {}
    @classmethod
    def make(cls, fname, codeblock=False):
        try:
            self = cls._cache[fname]
        except KeyError:
            self = cls._cache[fname] = cls(include(fname), codeblock)
        return self
    def __init__(self, code, codeblock):
        if codeblock:
            self.templ = '\n.. code-block:: scheme\n\n%s\n'
        else:
            self.templ = '\n::\n\n%s\n'
        self._dict = dict(
            (mo.group(1), mo.group(2)) for mo in SNIPPET.finditer(code))
    def get(self, name):
        return self.templ % indent(self._dict.get(name, name))
        
def scheme2rst(fname, codeblock=False):
    mo = BIGCOMMENT.search(file(fname).read())
    if mo is None: 
        sys.exit('No #| ..|# found!')
    text, code = mo.groups()
    snippet = SnippetExtractor(code, codeblock)
    def repl(mo):
        "replace SNIPPETNAME with the content of the snippet dictionary"
        return snippet.get(mo.group(1))
    def include_file(mo):
        return snippet.templ % indent(include(mo.group(1)))
    def include_snippet(mo):
        fname, name = mo.groups()
        snippet = SnippetExtractor.make(fname)
        return snippet.get(name)
    text = SNIPPETNAME.sub(repl, text)
    text = INCLUDESNIPPET.sub(include_snippet, text)
    text = INCLUDE.sub(include_file, text)
    rstfile = os.path.splitext(fname)[0] + '.rst'
    file(rstfile, 'w').write(text)
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
