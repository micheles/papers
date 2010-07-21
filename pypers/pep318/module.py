# module.py

"Magically decorated module"

import decorators,sys

thismodule=sys.modules[__name__]

class MyClass: "[Decorated]"

newmod=decorators.decorated(thismodule)


