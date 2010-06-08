import os, sys
from ms.plot_utils import GnuPlotter
DATADIR = os.path.expanduser('~/sp/equities-histories')
TEMPL = '''\
plot '-' using 1:2 with lines
$data
e'''

def make_graph(code, batch):
    gp = GnuPlotter(TEMPL, batch)
    lines = list(file(os.path.join(DATADIR, code)))[-500:]
    data = ''.join(lines)
    png_file = '/tmp/%s.png' % code
    gp.plot(locals())
    return png_file

if __name__ == '__main__':
    L = len(sys.argv) - 1
    if L == 1:
        batch = False
    elif L == 2:
        batch = bool(int(sys.argv[2])) # 0 or 1
    else:
        sys.exit('Examples: $ python simpleplotter.py "fri-gb;AVE"')
    make_graph(sys.argv[1], batch)
