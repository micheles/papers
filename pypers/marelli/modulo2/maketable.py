# maketable.py

# non graphic
N = 10
for i in range(1, N+1):
    for j in range(1, N+1):
        print "%4d" % (i*j),
    print

# HTML
def maketable(iterable, N):
    iterable = iter(iterable)
    print "<table border='1'>"
    stop = False
    while not stop:
        print "<tr>"
        for j in range(1, N+1):
            try: 
                print "<td>%s</td>" % iterable.next(),
            except StopIteration:
                print "<td></td>"
                stop = True
        print "</tr>"
    print "</table>"

import tempfile, webbrowser, os, sys

def showtable(iterable, N):
    stdout = sys.stdout
    fd, name = tempfile.mkstemp(suffix=".html")
    sys.stdout = os.fdopen(fd, "w")
    maketable(iterable, N)
    webbrowser.open(name)
    sys.stdout = stdout

showtable((i*j for j in range(1, N+1) for i in range(1, N+1)), N)



def get_files_with_ext(ext_set, d):
    if not isinstance(ext_set, set):
        ext_set = set([ext_set])
    for cwd, dirs, files in os.walk(d):
        for f in files:
            name, ext = os.path.splitext(f)
            if ext.lower() in ext_set:
                yield os.path.join(cwd, f)

class Picture(object):
    def __init__(self, pathname):
        self.pathname = pathname
        self.name = os.path.basename(pathname)
    def __str__(self):
        return "<img src=%r width=100>" % self.pathname
    
if sys.platform == 'win32': drive = "C:\\"
else: drive = "/"
showtable(map(Picture, get_files_with_ext(".jpg", drive)), N)


