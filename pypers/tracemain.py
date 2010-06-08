from oopp import ClsFactory,Traced,Reflective
def f1(x): return x     # nested functions 
def f2(x): return f1(x) # we want to trace
f1orf2=lambda k,v : v is f1 or v is f2
make=ClsFactory[Reflective,Traced.With(condition=f1orf2)]
traced=make('traced',globals())
traced.f2('hello!') # call traced.f2
