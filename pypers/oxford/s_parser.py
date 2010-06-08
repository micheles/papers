# s-expr parser

sexpr = """\
(html
(head
  (title HTML as s-expr example))
(body
  (h1  HTML as s-expr example)
  (a (href http://www.example.com) A link))
html)
"""

import re, inspect

def second_arg(func):
    args = inspect.getargspec(func)[0]
    if len(args) >= 2: return args[1]
    
class MetaParser(type):
    def __init__(cls, name, bases, dic):
        groups = []
        for n, f in dic.iteritems():
            if inspect.isfunction(f) and second_arg(f) == "pattern":
                groups.append("(?P<%s>%s)" % (n, f.func_defaults[0]))
        rx = re.compile("|".join(groups))
        
class BaseParser(object):
    __metaclass__ = MetaParser
    def __init__(self, sexpr):
        self.sexpr = sexpr

class SexprParser(BaseParser):
    def paren_beg(self, pattern=r"\("):
        return
    def paren_end(self, pattern=r"\)"):
        return
    def __iter__(self):
        seek = 0
        searcher = lambda : paren.search(self.text, seek) 
        while True:
            pass

