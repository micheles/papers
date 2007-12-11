import sys
from wsgiref.simple_server import make_server

# WSGI app
def simple_app(env, resp):
    resp('200 OK', [('Content-type','text/plain')])
    yield 'Hello world!\n'
    yield 1/0

def wrapped_app(app, env, resp):
    try:
        page = app(env, lambda s, h, e=None: None)
        print list(page)
    except:
        resp('500 ERR',  [("content-type","text/plain")], sys.exc_info())
        return ['err']
    else:
        resp('200 OK',  [("content-type","text/html")])
        return page

if __name__ == '__main__':
    make_server('', 8000, wrapped_app.__get__(simple_app)).serve_forever()
