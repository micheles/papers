"Remove unwanted .py files"

import os

def killable(name):
    preserve='test.py', 'oopp.py', 'remove.py', 'EXECUTEME.py'
    return name.endswith('.py') and not name.startswith('pro') and \
           name not in preserve

for f in os.listdir('.'):
    if killable(f): os.remove(f)
