# testnoconflict.py
from noconflict_alex import classmaker

errormsg23 = "multiple bases have instance lay-out conflict"
errormsg24 = """Error when calling the metaclass bases
    multiple bases have instance lay-out conflict"""
  
try:
  set
except NameError: # we are using Python 2.3
  errormsg = errormsg23
else: # we are using Python 2.4
   errormsg = errormsg24
   
# First, the trivial cases

#single old-style-would-be class 
class C: 
  __metaclass__ = classmaker()

# here needed_metas = ()
assert C.__class__ is type

#single new style class 
class C(object): 
  __metaclass__ = classmaker()

# here needed_metas = (type,)
assert C.__class__ is type

# class inheriting from old-style

class O: pass # old-style

class O_: pass # old-style

try:
  class C(O): 
    __metaclass__ = classmaker()
except TypeError,e:
  assert str(e) == "a new-style class can't have only classic bases"

try:
  class C(O, O_): 
    __metaclass__ = classmaker()
except TypeError,e:
  assert str(e) == "a new-style class can't have only classic bases"

# inheriting from both old style and new style works

class C(O, object): 
  __metaclass__ = classmaker()

# here needed_metas = (type,)
assert type(C) is type

# the simplest non-trivial case (M_A and M_B)

class M_A(type): pass
class M_B(type): pass
class A: __metaclass__ = M_A
class B: __metaclass__ = M_B

class C(A,B):
  __metaclass__ = classmaker()

# here needed_metas = (M_A, M_B)
assert C.__class__.__name__ == "_M_AM_B"

# injecting M_A from left

class C(B):
  __metaclass__ = classmaker((M_A,))

# here needed_metas = (M_A, M_B)
assert C.__class__.__name__ == "_M_AM_B"

# injecting M_B from right

class C(A):
  __metaclass__ = classmaker(right_metas = (M_B,))

# here needed_metas = (M_A, M_B)
assert C.__class__.__name__ == "_M_AM_B"


# redundant injections

class C:
  __metaclass__ = classmaker (left_metas = (M_B,), right_metas = (M_A, M_B,))

# here needed_metas = (M_B, M_A), the last M_B is correctly removed
assert C.__class__.__name__ == "_M_BM_A"

# composing an old-style class and a metaclass

class O: pass

try:
  class C(O, M_A):
    __metaclass__ = classmaker()
except TypeError, e:
  # here needed_metas = (type, )
  assert str(e) == errormsg


# the other way around

try:
  class C(M_A, O):
    __metaclass__ = classmaker()
except TypeError, e:
  # here needed_metas = (type, )
  assert str(e) == errormsg

# composing an new-style class and a metaclass 

class N(object): pass

try:
  class C(N, M_A):
    __metaclass__ = classmaker()
except TypeError, e:
  # here needed_metas = (type, )
  assert str(e) == errormsg


# the other way around

try:
  class C(M_A, N):
    __metaclass__ = classmaker()
except TypeError, e:
  # here needed_metas = (type, )
  assert str(e) == errormsg

# composing a non-trivial class and a metaclass

class C(B, M_A):
  __metaclass__ = classmaker()

# here needed_metas = (M_B,)
assert C.__class__ is M_B

# the other way around

class C(M_A, B):
  __metaclass__ = classmaker()

# here needed_metas = (M_B,)
assert C.__class__ is M_B

# a more bizarre hierarchy

class C(B, M_B):
  __metaclass__ = classmaker()

# here needed_metas = (M_B,)
assert C.__class__ is M_B
# C.__mro__ == [C, B, M_B, type, object]

# changing the order

class C(M_B, B):
  __metaclass__ = classmaker()

# here needed_metas = (M_B,)
assert C.__class__ is M_B
#  C.__mro__ == [C, M_B, type, B, object]


# meta-metaclasses

class MM_X(type): pass
class MM_Y(type): pass

class M_X(type): __metaclass__ = MM_X
class M_Y(type): __metaclass__ = MM_Y

class X(type): __metaclass__ = M_X
class Y(type): __metaclass__ = M_Y

class Z(X,Y): __metaclass__ = classmaker()

# here needed_metas = (M_X, M_Y)

assert Z.__class__.__name__ == "_M_XM_Y"

# in order to construct _M_XM_Y classmaker has to
# construct _MM_XMM_Y first:

assert Z.__class__.__class__.__name__ == "_MM_XMM_Y"

class C(Z, B): __metaclass__ = classmaker()

# here needed_metas = (_M_XM_Y, M_B)

# composition of many metaclasses

class M_A(type): pass
class M_B(type): pass
class M_C(M_B): pass 
class A: __metaclass__ = M_A
class B: __metaclass__ = M_B

class C: __metaclass__ = M_C
class D: __metaclass__ = M_B

class E(A, B):
  __metaclass__ = classmaker()

# here needed_metas = (M_A, M_B)
assert E.__class__.__name__ == "_M_AM_B"

class F(C):
  __metaclass__ = classmaker()

# here needed_metas = (M_A, M_B)

assert F.__class__.__name__ == "M_C"

class G(E, F):
  __metaclass__ = classmaker()
  
# here needed_metas = (_M_AM_B, M_C)
assert G.__class__.__name__ == "__M_AM_BM_C"

## Test for C-coded metaclasses, i.e. Zope ExtensionClass

try: # is Zope installed?
    from OFS.Folder import Folder # an instance of Zope ExtensionClass
    
    class C(Folder): # no problem here
      __metaclass__ = classmaker()

    assert C.__class__.__name__ == "ExtensionClass"

    try:
      class C(Folder): 
        __metaclass__ = classmaker((M_A,))
    except TypeError, e:
      e[1] == 'Incompatible root metatypes'
      
except ImportError:
    pass # will skip the ExtensionClass test
