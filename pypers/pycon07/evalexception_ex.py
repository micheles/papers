from wsgiref.simple_server import make_server
from paste.evalexception import EvalException

a, b = 1,0

def app(env, resp):
    resp('200 OK', [('Content-type', 'text/html')])
    return [str(a/b)]

if __name__ == '__main__':
    make_server('', 9090, EvalException(app)).serve_forever()
