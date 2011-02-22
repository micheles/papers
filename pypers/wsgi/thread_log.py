import logging, threading, sys

class C(object):
    def __init__(self):
        print threading.currentThread()
    def __del__(self):
        logger.warn('%s deleted %r' % (threading.currentThread(), self))
        
if __name__ == '__main__':
    logger = logging.getLogger()
    logger.basicConfig(file='/tmp/x.log')
    c = C()
    
