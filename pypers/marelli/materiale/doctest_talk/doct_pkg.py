import test_pkg, doctest, os

import sys
from ms.file_utils import ifiles

pkg = __import__("test_pkg")

pkg_name = pkg.__name__
csd = os.path.dirname(test_pkg.__path__[0]) # current search directory
os.chdir(csd)

print "Testing package", pkg_name

total_fail, total_ok = 0, 0
for f in ifiles(pkg_name, lambda f: f.endswith(".py"), abs_path=True):
    f = f[:-3].replace("/", ".")
    fail, ok = doctest.testmod(__import__(f, globals(), locals(), [f]))
    total_fail += fail
    total_ok += ok
print "Failed %s, passed %s" % (total_fail, total_ok)
# doctest.testmod(test_pkg) # only tests __init__
# doctest.run_docstring_examples(test_pkg, globals()) # idem
