# interp.py

import UserDict

class Chainmap(UserDict.DictMixin):
    """Combine multiple mappings for sequential lookup. Raymond Hettinger,
    http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/305268 """

    def __init__(self, *maps):
        self._maps = maps

    def __getitem__(self, key):
        for mapping in self._maps:
            try:
                return mapping[key]
            except KeyError:
                pass
        raise KeyError(key)



import sys
from string import Template

def interp(text, repldic=None, safe_substitute=True):
    caller = sys._getframe(1)
    if repldic:
        mapping = Chainmap(repldic, caller.f_locals, caller.f_globals)
    else:
        mapping = Chainmap(caller.f_locals, caller.f_globals)
    t  = Template(text)
    if safe_substitute:
        return t.safe_substitute(mapping)
    else:
        return t.substitute(mapping)
   
## Example:

language="Python"

def printmsg():
    opinion = "favorite"
    print interp("My $opinion language is $language.")


