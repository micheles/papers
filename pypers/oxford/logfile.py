# logfile.py

import subprocess

def memoize(func):
    memoize_dic = {}
    def wrapped_func(*args):
        if args in memoize_dic:
            return memoize_dic[args]
        else:
            result = func(*args)
            memoize_dic[args] = result
            return result
    wrapped_func.__name__ = func.__name__
    wrapped_func.__doc__ = func.__doc__
    wrapped_func.__dict__ = func.__dict__
    return wrapped_func

class Memoize(type): # Singleton is a special case of Memoize
    @memoize
    def __call__(cls, *args):
        return super(Memoize, cls).__call__(*args)

class LogFile(file):
    """Open a file for append."""
    __metaclass__ = Memoize
    def __init__(self, name = "/tmp/err.log"):
        self.viewer_cmd = 'xterm -e less'.split()
        super(LogFile, self).__init__(name, "a")

    def display(self, *ls):
        "Use 'less' to display the log file in a separate xterm."
        print >> self, "\n".join(map(str, ls)); self.flush()
        subprocess.call(self.viewer_cmd + [self.name])

    def reset(self):
        "Erase the log file."
        print >> file(self.name, "w")

if __name__ == "__main__": # test
    print >> LogFile(), "hello"
    print >> LogFile(), "world"
    LogFile().display()


