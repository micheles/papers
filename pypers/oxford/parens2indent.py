# parens2indent.py
"""A simple s-expression parser."""

import re

def parse(sexpr):
    position = 0
    nesting_level = 0
    paren = re.compile(r"(?P<paren_beg>\()|(?P<paren_end>\))")
    while True:
        match = paren.search(sexpr, position)
        if match:
            yield nesting_level, sexpr[position: match.start()]
            if match.lastgroup == "paren_beg":
                nesting_level += 1
            elif match.lastgroup == "paren_end":
                nesting_level -= 1
            position = match.end()
        else:
            break

def parens2indent(sexpr):
    for nesting, text in parse(sexpr.replace("\n", "")):
        if text.strip():  print " "*nesting, text

parens2indent( """\
(html (head (title Example)) (body (h1 s-expr formatter example)
(a (href http://www.example.com) A link)))""")


