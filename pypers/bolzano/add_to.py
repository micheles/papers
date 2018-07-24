import sys
def add_to(obj):
    def wrapper(f):
        setattr(obj, f.__name__, f)
        return f
    return wrapper

###################################

def obj(): pass

@add_to(obj)
def identity(x):
    return x

print identity
print obj.identity
