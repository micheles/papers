"""
How try .. finally works:
CTRL-C is caught, CTRL-Break is NOT
"""
import time
F = "x.txt"

f = file(F, "w")
try:
    for i in range(10):
        print >> f, "line %s" % (i + 1)
        time.sleep(1)
finally:
    f.close()
    print "File %r was closed correctly. Current content:" % F
    for line in file(F): print line,
    
