from wsgiref.simple_server import make_server

# WSGI app
def simple_app(env, resp):
    resp('200 OK', [('Content-type','text/plain')])
    return ['Hello world!\n']

# error management
def robust_app(env, resp): # the page has to be created fully *before* sending the response
    try:
        status = "200 OK"
        response_headers = [("content-type","text/plain")]
        resp(status, response_headers)
        return ["normal body goes here"]
    except KeyboardError:
        pass
    except:
        status = "500 ERR"
        response_headers = [("content-type","text/plain")]
        resp(status, response_headers, sys.exc_info())
        return ["error body goes here"]
                                            
# Middleware
upper = lambda x: (e.upper() for e in x)
def upper_middleware(app):
    return lambda env, resp: upper(app(env, resp))




if __name__ == '__main__':
    make_server('', 8000, upper_middleware(simple_app)).serve_forever()
