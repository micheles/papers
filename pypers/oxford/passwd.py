class User(object):
    def __init__(self, un, pw):
        self.un, self.pw = un, pw

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

   
class SpecificUser(User):
    pw = cryptedAttribute("b")
    


u = User("michele", "secret")
print u.un, u.pw

su = SpecificUser("michele", "secret")
print su.un, su.pw

print su._pw
