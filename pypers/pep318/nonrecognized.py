# nonrecognized.py

"Classes and functions inside a function are not decorated"

import decorators; decorators.enable()

def outer():

    class C(object):
        def f():
            "[staticmethod]"

    def g():
        "[staticmethod]"

    # testing

    assert isinstance(C.__dict__['f'],decorators.staticmethod)
    assert not isinstance(g,decorators.staticmethod)

outer()


