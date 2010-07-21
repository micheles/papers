import math
import psyco
psyco.full()
def erfc(x):
    exp = math.exp
    p  =  0.3275911
    a1 =  0.254829592
    a2 = -0.284496736
    a3 =  1.421413741
    a4 = -1.453152027
    a5 =  1.061405429
    t = 1.0 / (1.0 + p*x)
    erfcx = ( (a1 + (a2 + (a3 +
                          (a4 + a5*t)*t)*t)*t)*t ) * exp(-x*x)
    return erfcx
def main():
    erg = 0.0
    for i in xrange(1000000):
        erg += erfc(0.456)
    print "%f" % erg
main()
