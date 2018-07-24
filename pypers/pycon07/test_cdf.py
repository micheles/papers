from scipy.integrate import quad
from delta_dist import cdf, delta

def cdf(d):
    d2 = d*d
    Cd = gamma(1.5+d2/2.)/gamma(.5)/gamma(1.+d2/2.)
    return lambda x: Cd*x*hyp2f1(.5, 1.5+d2/2., 1.5, -x*x)

print quad(delta(.2), 0., 1.0)
print cdf(.2)(1.0)
