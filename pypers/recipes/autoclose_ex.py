import logging
from autoclose import AutoClose

class C(AutoClose):
   def __init__(self, id):
       self.id = id
   def close(self):
       logging.warn('closing object %s' % self.id)
       super(C, self).close()

class D(C):
   pass


if __name__ == '__main__':
   c1 = C(1)
   c2 = C(2)
   d3 = D(3)
