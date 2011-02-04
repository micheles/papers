import MySQLdb, subprocess

def percent(n, ntot):
    return float(n)/ntot * 100

def gen_histo(cx, kind, datafile):
    cu = cx.cursor()
    cu.execute("""select %s, count(*) from books group by %s
    order by %s;""" % (kind, kind, kind))
    books_per_kind = cu.fetchall()
    xtics = []; i = 0
    f = file(datafile, "w")
    for k, counts in books_per_kind:
        print >> f, counts
        xtics.append("%r %s" % (k, i))
        i += 1
    yield "set term png"
    yield "set output '%s.png'" % datafile[:-4]
    yield "set style fill solid"
    yield "set style fill border 2"
    yield "set xrange [-1:15]"
    yield "set xtics (%s)" % ", ".join(xtics)
    yield "plot %r with boxes" % datafile

def make_plot(cx, kind, datafile):
    gnuplot = subprocess.Popen(["gnuplot"], stdin=subprocess.PIPE)
    # for line in gen_histo(cx, kind, datafile): print line
    gnuplot.communicate("\n".join(gen_histo(cx, kind, datafile)))
                        
if __name__ == "__main__":
    cx = MySQLdb.connect(db="books")
    make_plot(cx, "nation", "nation.dat")
