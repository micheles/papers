# multilingual.py

import sys
from descriptor import AttributeDescriptor

class MultilingualAttribute(AttributeDescriptor):
    """When a MultilingualAttribute is accessed, you get the translation 
    corresponding to the currently selected language.
    """
    def __init__(self, **translations):
        self.trans = translations
    def get_from_class(self, cls):
        return self.trans[getattr(cls, "language", None) or
                         sys.modules[cls.__module__].language]
    def get_from_obj(self, obj):
        return self.trans[getattr(obj, "language", None) or
                         sys.modules[obj.__class__.__module__].language]
     

language = "en"

# a dummy User class
class DefaultUser(object):
    def has_permission(self):
        return False
   
class WebApplication(object):
    error_msg = MultilingualAttribute(
        en="You cannot access this page",
        it="Questa pagina non e' accessibile",
        fr="Vous ne pouvez pas acceder cette page",)
    user = DefaultUser()
    def __init__(self, language=None):
        self.language = language or getattr(self.__class__, "language", None)
    def show_page(self):
        if not self.user.has_permission():
            return self.error_msg


app = WebApplication()
assert app.show_page() == "You cannot access this page"

app.language = "fr"
assert app.show_page() == "Vous ne pouvez pas acceder cette page"

app.language = "it"
assert app.show_page() == "Questa pagina non e' accessibile"

app.language = "en"
assert app.show_page() == "You cannot access this page"


