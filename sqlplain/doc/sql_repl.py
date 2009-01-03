"""
usage: sql_repl <dbname>
"""
# alias sql_repl="rlwrap python ~/gcode/sqlplain/doc/sql_repl.py"

import os, sys, subprocess                              
from sqlplain import lazyconnect

def less(text):
    "Send a text to less via a pipe"
    # -c clear the screen before starting less
    po = subprocess.Popen(['less', '-c'], stdin=subprocess.PIPE)
    try:
        po.stdin.write(text)
        po.stdin.flush()
    except IOError:
        pass
    po.stdin.close()
    po.wait()
                                                                                
class Console(object):
    "A textual console to interact with a database"
    
    def __init__(self, dbname, input_src=sys.stdin):
        self.db = lazyconnect(dbname)
        self.input_src = input_src
        self.prompt = '%s> ' % self.db.name
        
    def sql_eval(self, code):
        rows = self.db.execute(code)
        out = ['Return %d rows' % len(rows), '\t'.join(rows.header)]
        for row in rows:
            out.append('\t'.join(map(str, row)))
        return '\n'.join(out)
            
    def readcode(self):
        lines = []
        while True:
            sys.stdout.write(self.prompt)
            sys.stdout.flush()
            line = self.input_src.readline()
            if not line:
                raise EOFError
            lines.append(line)
            if line.endswith(';\n'):
                return '\n'.join(lines)
            
    def repl(self):
        while True:
            try:
                code = self.readcode()
                less(self.sql_eval(code))
            except EOFError:
                break
            except Exception, e:
                print e.__class__.__name__, str(e)
                
if __name__ == '__main__':
    try:
        alias = sys.argv[1]
    except IndexError:
        sys.exit(__doc__)
    Console(alias).repl()
