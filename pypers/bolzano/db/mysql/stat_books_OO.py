import MySQLdb, subprocess, os, memoize

def percent(n, ntot):
    return float(n)/ntot * 100

class BookPlotter(object):
    def __init__(self, cx):
        self.cx = cx

    @ memoize.memoize
    def plot(self, kind, datafile):
        gnuplot = subprocess.Popen(["gnuplot"], stdin=subprocess.PIPE)
        gnuplot.communicate("\n".join(self.gen_histo(kind, datafile)))
        
    def gen_histo(self, kind, datafile):
        cu = self.cx.cursor()
        cu.execute("""select %s, count(*) from books group by %s
        order by %s;""" % (kind, kind, kind))
        books_per_kind = cu.fetchall()
        xtics = []; i = 0
        f = file(datafile, "w")
        for k, counts in books_per_kind:
            print >> f, counts
            xtics.append("%r %s" % (k, i))
            i += 1
        self.imagefile = os.path.abspath(datafile[:-4] + ".png")
        yield "set term png"
        yield "set output %r" % self.imagefile
        yield "set style fill solid"
        yield "set style fill border 2"
        yield "set xrange [-1:15]"
        yield "set xtics (%s)" % ", ".join(xtics)
        yield "plot %r with boxes" % datafile
        
    def show(self):
        subprocess.call(["display", self.imagefile])
                        
if __name__ == "__main__":
    plotter = BookPlotter(MySQLdb.connect(db="books"))
    plotter.plot("genre", "genre.dat")
    plotter.show()
