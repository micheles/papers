from wsgiref.util import shift_path_info
from paste.urlparser import StaticURLParser

class Dispatcher(object):
    html = '''
<html>
<head>
<style type="text/css" src="/static/themes/jqModal.css"> </style>
<script type="text/javascript" src="/static/jquery.pack.js"></script> 
<script type="text/javascript" src="/static/jquery.jqGrid.js"></script> 
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

    def __init__(self, baseapp, static=None):
        self.pages = {}
        if static:
            for name, dirpath in static.iteritems():
                self.pages[name]=StaticURLParser(directory=dirpath)
        self.add('', baseapp)

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
        except (TypeError, ValueError), e:
            assert callable(app) # assume app is valid WSGI application
        else: # create an application displaying the page
            def app(env, resp):
                resp('200 OK', [('Content-type', 'text/html')])
                return [self.html % (js, body)]
            app.__name__ = name
        self.pages[name] = app

