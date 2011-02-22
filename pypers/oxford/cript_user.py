# cript_user.py

from crypt import crypt

def cryptedAttribute(seed):
    def get(self):
        return getattr(self, "_pw", None)
    def set(self, value):
        self._pw = crypt(value, seed)
    return property(get, set)
   
class User(object):
    pw = cryptedAttribute("a")
    def __init__(self, un, pw):
        self.un, self.pw = un, pw


