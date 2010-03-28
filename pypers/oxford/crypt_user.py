# crypt_user.py

class User(object):
    def __init__(self, username, password):
        self.username, self.password = username, password



from crypt import crypt

def cryptedAttribute(seed="x"):
    def get(self):
        return getattr(self, "_pw", None)
    def set(self, value):
        self._pw = crypt(value, seed)
    return property(get, set)
   
User.password = cryptedAttribute()


