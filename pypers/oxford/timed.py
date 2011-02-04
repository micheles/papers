# timed.py

import sys, time

class Timed(object):
    """Decorator factory: each decorator object wraps a function and 
    executes it many times (default 100 times).
    The average time spent in one iteration, expressed in milliseconds, 
    is stored in the attributes wrappedfunc.time and wrappedfunc.clocktime,
    and displayed into a log file which defaults to stdout.
    """
    def __init__(self, repeat=100, logfile=sys.stdout):
        self.repeat = repeat
        self.logfile = logfile
    def __call__(self, func):
        def wrappedfunc(*args, **kw):
            fullname = "%s.%s ..." % (func.__module__, func.func_name)
            print >> self.logfile, 'Executing %s' % fullname.ljust(30),
            time1 = time.time()
            clocktime1 = time.clock()
            for i in xrange(self.repeat):
                res = func(*args,**kw) # executes func self.repeat times
            time2 = time.time()
            clocktime2 = time.clock()
            wrappedfunc.time = 1000*(time2-time1)/self.repeat
            wrappedfunc.clocktime = 1000*(clocktime2 - clocktime1)/self.repeat
            print >> self.logfile, \
                  'Real time: %s ms;' % self.rounding(wrappedfunc.time),
            print >> self.logfile, \
                  'Clock time: %s ms' % self.rounding(wrappedfunc.clocktime)
            return res
        wrappedfunc.func_name = func.func_name
        wrappedfunc.__module__ = func.__module__
        return wrappedfunc
    @staticmethod
    def rounding(float_):
        "Three digits rounding for small numbers, 1 digit rounding otherwise."
        if float_ < 10.:
            return "%5.3f" % float_
        else:
            return "%5.1f" % float_
   

