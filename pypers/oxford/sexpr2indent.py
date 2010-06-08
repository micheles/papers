# sexpr2indent.py
"""A simple s-expression formatter."""

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

def sexpr_indent(sexpr):
    for nesting, text in parse(sexpr.replace("\n", "")):
        if text.strip():  print " "*nesting, text


