def __init__(self):
    print('calling __init__')
    super().__init__()

class C(object):
    __init__ = __init__

if __name__ == '__main__':
    c = C()
