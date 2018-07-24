from oopp import codeprocess
wrongcode=r'''
"""Code processing example: replaces 'Print' with 'print' except in
comments and literal strings"""
Print "This program prints \"Hello World!\"" # look at this line!
'''
fixPrint=lambda s: s.replace('Print','print')
validcode=codeprocess(wrongcode,fixPrint)
print 'Source code:\n',validcode
print 'Output:\n'; exec validcode

