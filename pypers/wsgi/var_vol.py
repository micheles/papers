"""
Compare the approximated var-volatility relation with the exact
var-volatility relation for the delta-distribution. The result
is

var95 = 1.0-1.2 sigma
var99 = 2.0-2.4 sigma
"""

from scipy import sqrt, pi
from scipy.special import gamma
from scipy.integrate import quad
from scipy.optimize import brentq
from pylab import plot, show, arange, text

class deltadist(object):
    def __init__(self, delta, sigma):
        self.d = delta
        self.s = sigma
        self.d2 = delta * delta
        self.s2 = sigma *sigma
        self.ds2 = self.d2 * self.s2
        self.a = (2.+ self.d2)/2.
        self.b = (3. + self.d2)/2.
        self.C = gamma(self.b)/gamma(self.a)/gamma(.5)*self.ds2**self.a
    def __call__(self, x):
        return self.C/(x*x + self.ds2)**self.b

class deltacumdist(object):
    def __init__(self, delta, sigma):
        self.delta = deltadist(delta, sigma)
    def __call__(self, x):
        return quad(self.delta, 0., x)[0]+.5

class largepdist(object):
    def __init__(self, delta, sigma):
        self.d = delta
        self.s = sigma
        self.d2 = delta * delta
        self.s2 = sigma *sigma
        self.ds = self.d * self.s
        self.a = (2.+ self.d2)/2.
        self.b = (3. + self.d2)/2.
        self.c = 1./(2.+ self.d2)
        self.C = gamma(self.b)/gamma(self.a)/gamma(.5)
        
    def __call__(self, p):
        return self.ds*(2.*self.a*(1-p)/self.C)**(-self.c)

class largeXdist(object):
    def __init__(self, delta, sigma):
        self.d = delta
        self.s = sigma
        self.d2 = delta * delta
        self.s2 = sigma *sigma
        self.ds = self.d * self.s
        self.a = (2.+ self.d2)/2.
        self.b = (3. + self.d2)/2.
        self.c = 2.+ self.d2
        self.C = gamma(self.b)/gamma(self.a)/gamma(.5)/self.c      
    def __call__(self, X):
        return 1.-self.C*(self.ds/X)**self.c


d61 = deltadist(.6, 1.)
c61 = deltacumdist(.6, 1.)
s61 = largepdist(.6, 1.)
l61 = largeXdist(.6, 1.)


print s61(.95)
print s61(.99)

x = arange(10, 40, .1)

plot(x, map(deltacumdist(0.1, 1.),x), color='red')
plot(x, map(deltacumdist(1.0, 1.),x), color='green')
plot(x, map(deltacumdist(1.0, 1.),x), color='blue')
plot(x, map(deltacumdist(5.0, 1.),x), color='violet')

#plot(x, , color='black')

text(30, .9999, '$F_{0.1}(x)$', color='red')
text(30, .99985, '$F_{1.0}(x)$', color='green')
text(30, .9998, '$F_{5.0}(x)$', color='blue')

print '***'
print brentq(lambda x: c61(x)-.95, .1, 4.)
print brentq(lambda x: c61(x)-.99, .1, 4.)

show()
