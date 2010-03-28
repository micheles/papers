# paleo.py

class Homo(object):
    def can(self):
        print "<%s> can:" % self.__class__.__name__
        for attr in dir(self): 
            if attr.endswith('__can'): print getattr(self, attr)

class HomoHabilis(Homo): 
    __can = " - make tools"

class HomoSapiens(HomoHabilis): 
    __can = " - make abstractions"

class HomoSapiensSapiens(HomoSapiens): 
    __can = " - make art"

modernman = HomoSapiensSapiens()


