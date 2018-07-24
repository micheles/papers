from pylab import *
from scipy.special import gamma, erf, hyp2f1
from scipy.optimize import brentq

Cg = 1/sqrt(2*pi)
SQRT2 = sqrt(2.)

def gauss(x):
    return Cg*exp(-x*x)

def cdf_gauss(x):
    return erf(x/SQRT2)/2.+.5

# avg is a global variable
def delta(d):
    d2 = d*d
    Cd = gamma(1.5+d2/2.)/gamma(.5)/gamma(1.+d2/2.)
    return lambda x: Cd*d2**(1.+d2/2.)/(x*x + d2)**(1.5+d2/2.)

def cdf(d):
    d2 = d*d
    Cd = gamma(1.5+d2/2.)/gamma(.5)/gamma(1.+d2/2.)
    return lambda x: Cd*x/d*hyp2f1(.5, 1.5+d2/2., 1.5, -x*x/d2) + .5

def plot_cdf():
    x = arange(-5,5,.1)
    plot(x, cdf_gauss(x), color='black')
    plot(x, cdf(.2)(x), color='red')
    plot(x, cdf(1.)(x), color='green')
    plot(x, cdf(5.)(x), color='blue')

    text(1.5, .3, '$c_{gauss}\ :\ VAR_{95}=1.64\ \sigma$', color='black')
    text(1.5, .6, '$c_{0.2}\ :\ VAR_{95}=0.40\ \sigma$', color='red')
    text(1.5, .5, '$c_{1.0}\ :\ VAR_{95}=1.36\ \sigma$', color='green')
    text(1.5, .4, '$c_{5.0}\ :\ VAR_{95}=1.64\ \sigma$', color='blue')

    savefig('cdf-dist.png', dpi=72)
    show()
## you get VAR_{95}/vol from computation like
# brentq(lambda x: cdf(0.2)(x)-.95,.1,10)
# brentq(lambda x: cdf(1.0)(x)-.95,.1,10)
# brentq(lambda x: cdf(5.0)(x)-.95,.1,10)

def plot_delta():
    x = arange(-5,5,.1)
    plot(x, gauss(x), color='black')
    plot(x, delta(.1)(x), color='red')
    plot(x, delta(1.)(x), color='green')
    plot(x, delta(5.)(x), color='blue')

    text(4.5, 4, '$f_{0.2}(x)$', color='red')
    text(4.5, 3, '$f_{1.0}(x)$', color='green')
    text(4.5, 2, '$f_{5.0}(x)$', color='blue')
    text(4.5, 1, '$\phi(x)$', color='black')

    savefig('delta-dist.png', dpi=72)
    show()

if __name__ == '__main__':
    if '-c' in sys.argv[1:]:
        plot_cdf()
    else:
        plot_delta()

