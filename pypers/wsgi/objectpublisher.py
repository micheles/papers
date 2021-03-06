from wsgiref import util, simple_server

# page is any callable object returning HTML in chunks
class WSGIObjectPublisher(object):
    def __init__(self, root):
        self.root = root
    def __call__(self, env, resp):
        return self.getsubpage(self.root, env, resp)()
    def getsubpage(self, root, env, resp):
        script_name = util.shift_path_info(env)
        print script_name, '****', env['PATH_INFO']
        if not script_name: # We've arrived!
            resp('200 OK', [('content-type', 'text/html')])
            return root
        try:
            page = getattr(root, script_name)
        except AttributeError:
            resp('404 Not Found', [('content-type', 'text/plain')])
            return lambda : ['missing page %r' % script_name]
        exposed = getattr(page, 'exposed', False)
        if not exposed:
            resp('404 Not Found', [('content-type', 'text/plain')])
            return lambda : ['%r is not exposed!' % script_name]
        return self.getsubpage(page, env, resp)

if __name__ == '__main__':
    class Example(object):
        def __call__(self):
            yield '<h1>index page</h1>'
            yield 'goto <a href="./page1">page1</a><br/>'
            yield 'goto <a href="./page2">page2</a><br/>'
            yield 'goto <a href="subsite">subsite</a><br/>'
        def page1(self):
            yield 'page1'
        def page2(self):
            yield 'page2'
        page1.exposed = page2.exposed = True
    root = Example()
    root.subsite = Example()
    root.subsite.exposed = True
    app = WSGIObjectPublisher(root)
    from paste.auth.basic import AuthBasicHandler
    def authfunc(environ, username, password):
        return username == 'pippo' and password == 'lippo'
    app_with_auth = AuthBasicHandler(app, 'Test Realm', authfunc)
    simple_server.make_server('', 8000, app_with_auth).serve_forever()
