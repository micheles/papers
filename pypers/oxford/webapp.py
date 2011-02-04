# webapp.py

class WebApplication(object):
    def __getattr__(self, name):
        return name.capitalize()


app = WebApplication()

assert app.page1 == 'Page1'
assert app.page2 == 'Page2'


