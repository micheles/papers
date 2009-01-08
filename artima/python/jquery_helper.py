from wsgiref.util import shift_path_info
from paste.urlparser import StaticURLParser

class Dispatcher(object):
    html = '''
<html>
<head>
<script type="text/javascript" src="/static/jquery.pack.js">
</script> 
<script type="text/javascript">
$(document).ready(function(){
%s
});
</script>            
</head>
<body>
%s
</body>
<html>
''' # default HTML template

    def __init__(self, static, root):
        self.pages = dict(static=StaticURLParser(directory=static))
        self.add('', root)

    def __call__(self, env, resp):
        name = shift_path_info(env)
        app = self.pages.get(name)
        if not app:
            resp('404 ERR', [('Content-type', 'text/plain')])
            return ['Page %s not found' % name]
        return app(env, resp)

    def add(self, name, app):
        try: # look if app is a pair (html, js)
            body, js = app
        except ValueError:
            assert callable(app) # assume app is valid WSGI application
        else: # create an application displaying the page
            def page(env, resp):
                resp('200 OK', [('Content-type', 'text/html')])
                return [self.html % (js, body)]
            page.__name__ = name
        self.pages[name] = page

