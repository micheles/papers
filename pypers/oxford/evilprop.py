# evilprop.py

def convert2property(name, bases, d):
    return property(d.get('get'), d.get('set'),
                    d.get('del'),d.get('__doc__'))

class C(object):
    class x:
        """An evil test property"""
        __metaclass__ = convert2property
        def get(self):
            print 'Getting %s' % self._x
            return self._x
        def set(self, value):
            self._x = value
            print 'Setting to', value


