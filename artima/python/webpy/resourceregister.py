import re
import traceback
from UserDict import DictMixin
from wsgiref.util import shift_path_info

class ODict(DictMixin):
    "A simple ordered dict"
    def __init__(self, pairs):
        self._dict = {}
        self._keys = []
        for key, value in pairs:
            self[key] = value
    def __setitem__(self, key, value):
        if key not in self._dict:
            self._keys.append(key)
        self._dict[key] = value
    def __getitem__(self, key):
        return self._dict[key]
    def keys(self):
        return self._keys
    def __repr__(self):
        lst = ['%r: %r' % (n, v) for n, v in self.iteritems()]
        return '{%s}' % ', '.join(lst)
    
class HTTPError(Exception):
    def __init__(self, status, message):
        self.status = status
        self.message = message

# a small convenience function
def respond(resp, status, content_type, msg):
    if isinstance(msg, unicode):
        msg = msg.encode('utf8')
    resp(status, [('Content-type', '%s; charset=utf8' % content_type),
                  ('Content-length', str(len(msg)))])
    return [msg]

# return response(env, resp) # is the webob suggested pattern inside resources


# another convenience to call resources
def wsgicall(env, resp, resource, Request, args, debug=True):
    try:
        result = resource(Request(env), *args)
        if result is None:
            raise TypeError('%s returned None' % resource.__name__)
        elif isinstance(result, str):
            raise TypeError(
                '%s returned a string instead of a '
                'sequence of strings' % resource.__name__)
        output = ''.join(result) # str or unicode
    except HTTPError, err:
        return respond(resp, err.status, 'text/plain', err.message)
    except:
        if debug:
            return respond(resp, '500 ERR', 'text/plain',traceback.format_exc())
        raise # let the server manage the error in non debug mode
    return respond(resp, '200 OK', resource.content_type, output)
  
class ResourceDispatcher(object):
    """
    Mapping is a dictionary (regex, meth)->[resource|ResourceDispatcher]
    """
    def __init__(self, Request=None, debug=True):
        self.debug = debug
        if Request is None:
            import webob;  Request = webob.Request
        self.Request = Request
        self.mapping = ODict([])

    def __call__(self, env, resp):
        reqmeth = env['REQUEST_METHOD']
        script_name = env.get('SCRIPT_NAME', '')
        path_info = env.get('PATH_INFO', '')
        fullpath = script_name + path_info
        #print 'SCRIPT_NAME', script_name
        #print 'PATH_INFO', path_info
        for (regexp, meth), resource in self.mapping.iteritems():
            mo = re.match(regexp, path_info)
            if mo:
                if isinstance(resource, ResourceDispatcher):
                    # dispatch to the subdispatcher
                    shift_path_info(env)
                    return resource(env, resp) 
                elif meth and meth != reqmeth:
                    return respond(resp, '405 ERR', 'Method Not Allowed')
                return wsgicall(env, resp, resource, self.Request, 
                                mo.groups(), self.debug)
        return respond(resp, '404 Not Found', 'Unknown resource %r' % fullpath)

class ResourceRegister(object):
    """
    Each instance is a decorator. Each application adds a new RESTful resource.
    """
    def __init__(self, dec=lambda f: f, Request=None, debug=True):
        self.dec = dec
        self.app = ResourceDispatcher(Request, debug)
    
    def __call__(self, path_regex, content_type='text/plain'):
        def dec(func):
            name = func.__name__
            if name.endswith(('_GET', '_POST', '_PUT', '_DELETE')):
                chunks = name.split('_')
                name = '_'.join(chunks[:-1])
                meth = chunks[-1]
            else:
                meth = None
            newfunc = self.dec(func) # decorate the original function
            newfunc.path_regex = path_regex
            newfunc.content_type = content_type
            self.app.mapping[path_regex, meth] = newfunc
            return newfunc
        return dec

    def add(self, path_regex, app):
        self.app.mapping[path_regex, None] = app
        
if __name__ == '__main__':
    from wsgiref.simple_server import make_server
    
    resource = ResourceRegister()
    
    @resource('/b')
    def book_GET(req):
        yield 'something'
    
    @resource('/c')
    def book_POST(req):
        pass

    @resource('/d')
    def book_PUT(req):
        pass

    @resource('/e')
    def book_DELETE(req):
        pass

    subresource = ResourceRegister()

    @subresource('/1')
    def title_GET(req):
        return ['1']

    resource.add('/a', subresource.app)
    
    print resource.app.mapping
    
    make_server('', 8080, resource.app).handle_request()
