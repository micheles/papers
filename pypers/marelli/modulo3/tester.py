import os, cmd, config
from ms.file_utils import ifiles
from ms.concurrency import Popen
from operator import attrgetter
import sys, re

def strip_number(song, rx=re.compile(r"\s*\d\d?\. ")):
    return rx.sub("", os.path.basename(song))

class TestProcess(object):
    def __init__(self, number, name, popencmd):
        self.name = name
        self.number = number
        self.popencmd = popencmd
    def start(self):
        self.proc = Popen(self.popencmd)
    def stop(self):
        if self.is_running():
            self.proc.kill()
    def is_running(self):
        return hasattr(self, "proc") and self.proc.is_running()

def popencmd(song):
    if sys.platform == "win32":
        return "mplay32", "/play", "/close", song
    else:
        return "xterm", "-geometry", "70x14", "-e", "mpg123", song

def str2int(args):
    for arg in  args.split():
        try:
            i = int(arg)
        except ValueError:
            print "Warning: argument %s is not an integer, ignored" % arg
        else:
            yield i

class Tester(cmd.Cmd):
    use_rawinput = False
    prompt = ">>" + chr(0)
    def preloop(self):
        self.songs = list(ifiles(config.MUSICDIR,
                                 lambda f: f.endswith(".mp3")))
        self.tests = dict(
            [i, TestProcess(i, strip_number(song), popencmd(song))]
                      for i, song in enumerate(self.songs))
        self.started_tests = set()
        self.do_show()
        
    def running_tests(self):
        for test in self.started_tests:
            if test.is_running(): 
                yield test
                
    def run_test(self, testnumber):
        if testnumber in self.tests:
            test = self.tests[testnumber]
            if test.is_running():
                print "Test #%s is already running!" % test.number
            else:
                test.start()
                self.started_tests.add(test)
                print "test #%s started" % test.number
        else:
            print "There is no test #%s" % testnumber
            
    def stop_test(self, testnumber):
        if testnumber in (test.number for test in self.running_tests()):
            self.tests[testnumber].stop()
            print "Test #%s stopped." % testnumber
        else:
            print "There is no process #%s running" % testnumber
            
    def do_show(self, arg=""):
        if arg == "runnable":
            self.show_runnable()
        elif arg == "already_run":
            self.show_already_run()
        elif arg is "":
            self.show_already_run()
            self.show_runnable()
        else:
            print "Unknown argument %s" % arg

    def show_runnable(self):
        print "Runnable tests:"
        if set(self.tests.itervalues()) != self.started_tests:
            for test in self.tests.itervalues():
                if not test in self.started_tests:
                    print test.number, test.name
        else: # all tests have been started
            print "None"

    def show_already_run(self):
        print "Already run tests:"
        if not self.started_tests:
            print "None"
        else:
            for test in sorted(self.started_tests, key=attrgetter("number")):
                print test.number, test.name
            
    def do_run(self, args):
        run = map(self.run_test, str2int(args))
        if not run: map(self.run_test, range(len(self.tests))) # run all
        
    def do_stop(self, args):
        stop = map(self.stop_test, str2int(args))
        if not stop: self.postloop() # stop all
        
    def do_quit(self, arg):
        print "exiting ..."
        return True

    def postloop(self):
        for test in self.running_tests():
            self.stop_test(test.number)
            
if __name__ == "__main__":
    Tester().cmdloop()
