class with_attrs(object):
    def __init__(self, **kw):
        self.kw = kw
    def __call__(self, func):
        print "the decorator was called with %s" % self.kw 
        def wrapped_func():
            print "%s was called" % func.__name__
            return func()
        wrapped_func.__dict__.update(self.kw)
        return wrapped_func

@with_attrs(author="M.S.", date="2005-04-19")
def long_named_function():
    print "do something"
    return "ok"

#long_named_function = decorator(long_named_function)


print long_named_function.author
print long_named_function.date

print "-" * 77

print long_named_function()
