# customdec.py

from decorators import *  

class chattymethod(MethodDecorator):
    logfile=sys.stdout # default
    def get(self,obj,cls=None): # same signature as __get__
        self.logfile.write('calling %s from %s\n' % (self,obj or cls))
        return super(chattymethod,self).get(obj,cls)



class chattymethod2(chattymethod): 
    logfile=sys.stdout # default
    def __init__(self,objfunc):
        super(chattymethod2,self).__init__(objfunc)
        logfile=getattr(self.__func__,'logfile',None)
        if logfile: self.logfile=logfile 



class tracedmethod(MethodDecorator):
    "Descriptor class, converts a method in a traced method"
    indent=0; output=sys.stdout # defaults

    def __init__(self,objfunc):
        super(tracedmethod,self).__init__(objfunc)
        self.funcname=self.__func__.__name__
        output=getattr(self.__func__,'output',None) 
        if output: self.output=output # func.attr. -> dec.attr.

    def get(self,obj,cls):
        clsname=self.__klass__.__name__ # definition clas
        def tracedmeth(obj,*args,**kw):
            i=' '*self.indent # default indentation
            self.__class__.indent+=4 # increases indentation
            self.output.write("%sCalling '%s.%s' with arguments " % 
                             (i,clsname,self.funcname))
            self.output.write("%s%s ...\n" % (obj or '',str(args)+str(kw)))
            res=super(tracedmethod,self).get(obj,cls)(*args,**kw)
            self.output.write("%s'%s.%s' called with result: %s\n"
                               % (i,clsname,self.funcname,res))
            self.__class__.indent-=4 # restores default indentation
            return res
        return tracedmeth.__get__(obj,cls) # method wrapper



class Logged(ClassDecorator):
    output=sys.stdout
    def __init__(cls,name,bases,dic):
        super(Logged,cls).__init__(name,bases,dic)
        print >> cls.output,"%s created" % cls



from types import FunctionType  

class Traced(ClassDecorator):
    def __init__(cls,n,b,d):
        for name,func in d.iteritems():
            if isinstance(func,FunctionType): # modifies the docstring
                func.__doc__="[tracedmethod] " + (func.__doc__ or '')
        super(Traced,cls).__init__(n,b,d)



