from sqlplain.memoize import Memoize

class Forever(object):
    "Cache type for caches which are is never cleared"
    
class LongTime(object):
    "Cache type for caches which are rarely cleared"
    
class ShortTime(object):
    "Cache type for caches which are is often cleared"

@Memoize(ShortTime)
def f1():
    return 1

@Memoize(LongTime)
def f2():
    return 2

@Memoize(Forever)
def f3():
    return 3

def test_cache():
    assert not f1.cache
    f1()
    assert f1.cache == {(): 1}
    f1()
    assert f1.cache == {(): 1}
    f2()
    Memoize.clear(ShortTime)
    assert not f1.cache
    assert f2.cache == {(): 2}
