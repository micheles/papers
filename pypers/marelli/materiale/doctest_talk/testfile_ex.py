import doctest, unittest
suite = doctest.DocFileSuite("example.txt", package='ms.test')
unittest.TextTestRunner().run(suite)

# alternatively

#import types
#f = file("/home/micheles/md/scripts/ms/test/example.txt")
#mod = types.ModuleType("example", f.read())
#suite = doctest.DocTestSuite(mod)
#unittest.TextTestRunner().run(suite)
