class MyStr(str):
    def __new__(cls, arg):
        return str.__new__(cls, 1)


print repr(MyStr("world"))

print type(MyStr("world"))
