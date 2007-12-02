#!/usr/bin/python2.4
"""
usage: %prog [options]
-i, --input=/tmp: search directory
-o, --output=.: output directory
-s, --size: order by size
"""

import os, sys, cgi
from ms.optionparse import OptionParser, option_parser_from
from ms.html_utils import template, simplepage, printpage, cgiform, \
     getscriptname

@template()
def checkbox(option, flags):
    for flag in flags:
        name = flag.dest
        if not name or name == "submit": continue
        checked = ["", "checked"][getattr(option, name) or 0]
        yield "<tr>\n"
        yield "<td>%s</td>" % flag.help
        yield "<td><input type='checkbox' name=%r %s></td>" % (name, checked)
        yield "</tr>\n"
    
@template()
def entrybox(option, option_args):
    for o_arg in option_args:
        name = o_arg.dest
        if not name: continue
        val = getattr(option, name) or ""
        yield "<tr>"
        yield "<td>%s</td>" % o_arg.help
        yield "<td><input type='text' name=%r value=%r></td>" % (name, val)
        yield "</tr>"
         
@template()
def inputform(option, op):
    yield "<form action=%r>" % getscriptname()
    yield "<table summary='Input form'>"
    yield entrybox(option, op.get_option_args())
    yield checkbox(option, op.get_flags())
    yield "</table>"
    yield "<input type='submit' name='submit' value='confirm'>"
    yield "</form>"

def entryform(form):
    printpage(inputform(option, op), title="Example 1")

def exitform(form):
    printpage("Thank you for filling this form.")
    
if __name__ == "__main__":
    op = option_parser_from(__doc__)
    op.add_option("-S", "--submit", action="store_true", default=False,
                  help="Submit the form")
    option, args = op.parse_args()
    if option.submit: # script called from the command line
        os.environ["QUERY_STRING"] = "&submit=confirm" 
    cgiform(entryform, exitform)
    
