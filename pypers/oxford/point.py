# point.py

class NotWorkingPoint(tuple):
    def __init__(self, x, y):
        super(NotWorkingPoint, self).__init__((x,y))
        self.x, self.y = x, y



class Point(tuple):
    def __new__(cls, x, y):
        return super(Point, cls).__new__(cls, (x,y))
    def __init__(self, x, y):
        super(Point, self).__init__((x, y))
        self.x, self.y = x, y


