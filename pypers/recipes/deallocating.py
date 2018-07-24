import logging

class C(object):
    def __init__(self):
        logging.warn('Allocating resource ...')

    def __del__(self):
        logging.warn('De-allocating resource ...')
        print 'THIS IS NEVER REACHED!'

if __name__ == '__main__':
    c = C()
