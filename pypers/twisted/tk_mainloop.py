import sys, time
from ms.async_utils import \
     TkControlPanel, TkinterTask, ThreadedTask, TwistedTask
from ms.debug_utils import killme; killme()

def print_(sym):
    for i in range(50):
        time.sleep(.1)
        sys.stdout.write(sym)
        sys.stdout.flush()
        yield i
    print "task %r ended" % sym

def main_threaded(gen):
    t = ThreadedTask.generator(gen)
    for s in "+-*/":
        task = t(s)
        task.start()
        # print task
    raw_input("Press return to stop\n")
    ThreadedTask.stopAll()  
    raw_input("\nPress return to resume")
    ThreadedTask.startAll()  
    raw_input("Press return to exit\n")
    ThreadedTask.stopAll()  

def main_tkinter(gen):
    from Tkinter import Tk
    t = TkinterTask.generator(gen)
    root = Tk()
    for s in "+-*/":
        t(s).start()
    root.mainloop()

def main_twisted(gen):
    from twisted.internet import reactor
    t = TwistedTask.generator(gen)
    for s in "+-*/":
        t(s).start()
    reactor.run()

def main_control_panel(func):
    t = TkinterTask.generator(func)
    root = TkControlPanel()
    for s in "1234":
        task = t(s)
        task.start()
    root.mainloop()
    
if __name__ == "__main__":
    #main_threaded(print_)
    #main_tkinter(print_)
    #main_twisted(print_)
    main_control_panel(print_)
