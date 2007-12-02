
from oopp import *

wrongcode='''
r"""Code processing example: replaces 'Print' with 'print' except in
comments and literal strings"""
Print "Prints \"Hello World!\"" # look at this line
'''

fixPrint=lambda s: s.replace('Print','print')
print codeprocess(wrongcode,fixPrint)

print quotencode(wrongcode)
