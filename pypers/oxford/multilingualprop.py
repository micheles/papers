# multilingualprop.py

language = "en"

# a dummy User class
class DefaultUser(object):
    def has_permission(self):
        return False
   
def multilingualProperty(**trans):
    def get(self):
        return trans[self.language]
    def set(self, value):
        trans[self.language] = value 
    return property(get, set)

class WebApplication(object):
    language = language
    error_msg = multilingualProperty(
        en="You cannot access this page",
        it="Questa pagina non e' accessibile",
        fr="Vous ne pouvez pas acceder cette page",)
    user = DefaultUser()
    def __init__(self, language=None):
        if language: self.language = self.language
    def show_page(self):
        if not self.user.has_permission():
            return self.error_msg


