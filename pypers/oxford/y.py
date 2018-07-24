class Point(tuple):
    @staticmethod
    def __new__(cls, x, y):
        return tuple.__new__(cls, [x, y])
    def __init__(self, x, y):
        self.x = x
        self.y = y

p = Point(1,2)

print p.x, p.y
