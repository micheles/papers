def super():
    print("I am evil, you are NOT calling the supermethod!")

class C(object):
    def __init__(self):
        super().__init__()

if __name__ == '__main__':
    c = C() # prints "I am evil, you are NOT calling the supermethod!"
