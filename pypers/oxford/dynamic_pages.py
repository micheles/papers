class WebApplication(object):
    def __getattr__(self, name):
        return "Page %s" % name


app = WebApplication()

print app.page1
print app.page2
