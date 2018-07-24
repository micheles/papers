from pylab import *
from scipy.special import gamma, hyp2f1
from scipy.integrate import quad, Inf
from scipy.optimize import brentq

def hyp(x, a):
    return x*hyp2f1(.5, a, 1.5, -x*x)
        
Cg = 1/sqrt(2*pi)

def gauss(x):
    return Cg*exp(-x*x)

def delta(d2):
    Cd = gamma(1.5+d2/2.)/gamma(.5)/gamma(1.+d2/2.)
    return lambda x: Cd*d2**(1.+d2/2.)/(x*x + d2)**(1.5+d2/2.)

def getsol(x):
    def f(d2):
        Cd = gamma(1.5+d2/2.)/gamma(.5)/gamma(1.+d2/2.)
        return .999-2*Cd*hyp(x, (3.+d2)/2.)
    return math.sqrt(brentq(f, 0., 100.))

x = arange(.01,5.,.1)
sols = [getsol(d) for d in x]
plot(x, sols, color='red')
#plot(x, hyp(x, 1.7), color='blue')
#plot(x, quad(delta(.1), x, Inf), color='red')
#plot(x, quad(delta(1.), x, Inf), color='green')
#plot(x, quad(delta(5.), x, Inf), color='blue')

#text(4.5, 1.4, '$f_{0.1}(x)$', color='red')
#text(4.5, 1.3, '$f_{1.0}(x)$', color='green')
#text(4.5, 1.2, '$f_{5.0}(x)$', color='blue')

savefig('limit999.png', dpi=72)
show()
