import re
import cgi
import traceback
from UserDict import DictMixin
from wsgiref.util import shift_path_info
        
class HTTPResponse(Exception):
    "Should be instantiated with a Response object as argument"

class InvalidResource(TypeError):
    "Should be instantiated with a resource object as argument"

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

class SimpleRequest(object):
    "A poor man request class. You may want to use webob.Request instead." 
    def __init__(self, environ):
        self.environ = environ
        self.params = cgi.parse(environ['wsgi.input'], environ, 
                                keep_blank_values=True, strict_parsing=False)

class SimpleResponse(object):
    "A poor man response class. You may want to use webob.Response instead."
    def __init__(self, body, status='200 OK',
                 content_type='text/plain', headerlist=None):  
        if isinstance(body, unicode):
            body = body.encode('utf8')
        elif not isinstance(body, str):
            raise TypeError('body must be str or unicode, got %r' % body)
        self.body = body
        self.status = status
        self.content_type = '%s; charset=utf8' % content_type
        self.headerlist = headerlist or [
            ('Content-type', self.content_type),
            ('Content-length', str(len(self.body)))]
    def __call__(self, env, start_resp):
        start_resp(self.status, self.headerlist)
        return [self.body]

def call(resource, req, args, content_type, debug):
    """
    Utility calling the resource and returning a WSGI app. The resource
    can return a sequence of strings or raise an HTTPResponse, which is
    trapped, or an unexpected exception, which is trapped if debug=True
    and re-raised otherwise. The only way to change the headers for the
    resource is to raise an HTTPResponse by using http.respond.
    """
    try:
        result = resource(req, *args)
        if result is None:
            raise TypeError('%s returned None' % resource.__name__)
        elif isinstance(result, str):
            raise TypeError(
                '%s returned a string instead of a '
                'sequence of strings' % resource.__name__)
        output = ''.join(result) # str or unicode
    except HTTPResponse, exc: # expected exception
        res = exc.args[0]
    except: # unexpected exception
        if debug:
            res = SimpleResponse(traceback.format_exc(), '500 ERR')
        raise # let the server manage the error in non debug mode
    else: # success
        res = SimpleResponse(output, '200 OK', content_type)
    return res

class HttpResourceManager(object):
    """
    A WSGI dispatcher based on an ordered mapping
    (regex, content_type, meth) -> [resource | HttpResourceManager].
    You should specify the Request and Response class. 
    If a nontrivial decorator is given, it is applied to the resources.
    If debug is True, broken resources return the full traceback.
    """
    valid_content_types = dict(plain='text/plain',
                               html='text/html',
                               xml='text/xml',
                               json='application/json')
    valid_methods = ['GET', 'POST', 'PUT', 'DELETE']
    
    def __init__(self, Request=SimpleRequest, Response=SimpleResponse, 
                 dec=lambda f: f, debug=True):
        self.Request = Request
        self.Response = Response
        self.dec = dec
        self.debug = debug
        self._mapping = ODict([])
 
    def inspectresource(self, obj):
        "Return basename, content_type and method or raise InvalidResource"
        if not callable(object) or not hasattr(obj, '__name__'):
            raise InvalidResource(obj)
        chunks = obj.__name__.split('_')
        if len(chunks) < 2: # there must be at least one underscore
            raise InvalidResource(obj)
        basename = '_'.join(chunks[:-1])
        has_method_suffix = chunks[-1] in self.valid_methods
        if has_method_suffix and chunks[-2] in self.valid_content_types:
            return basename, chunks[-2], chunks[-1]
        elif chunks[-1] in self.valid_content_types:
            return basename, chunks[-1], None
        else:
            raise InvalidResource(obj)
 
    def __call__(self, env, start_resp):
        "Dispatch logic"
        reqmeth = env['REQUEST_METHOD']
        script_name = env.get('SCRIPT_NAME', '')
        path_info = env.get('PATH_INFO', '')
        fullpath = script_name + path_info
        for (regexp, ctype, meth), resource in self._mapping.iteritems():
            mo = re.match(regexp, path_info)
            if mo:
                try:
                    self.inspectresource(resource)
                except InvalidResource: # assume it is a WSGI app
                    shift_path_info(env) # subdispatch
                    res = resource; break
                if meth and meth != reqmeth:
                    res = SimpleResponse('Method Not Allowed', '405 ERR')
                else: # call the resource
                    req = self.Request(env)
                    res = call(resource, req, mo.groups(), ctype, self.debug)
                break
        else: # no match, no break
            res = SimpleResponse('Unknown resource %r' % fullpath, 
                                 '404 Not Found')
        return res(env, start_resp)

    def add(self, app, path_regex, ctype='text/html', meth=None):
        "Add a WSGI application to the internal mapping"
        if (path_regex, ctype, meth) in self._mapping:
            msg = 'You are overriding the URL %s' % path_regex
            if meth:
                msg +=' (%s)' % meth
            raise KeyError(msg)
        self._mapping[path_regex, ctype, meth] = app

    def resource(self, path_regex):
        "Return a resource decorator"
        assert path_regex.startswith('/'), path_regex
        def dec(func):
            basename, ctype, meth = self.inspectresource(func)
            newfunc = self.dec(func) # decorate the original function
            newfunc.path_regex = path_regex
            self.add(newfunc, path_regex, ctype, meth)
            return newfunc
        return dec

    def respond(self, body, status='200 OK', content_type=None, 
                headerlist=None, **kw):
        "Raise a HTTPResponse"
        d = dict(body=body, status=status, content_type=content_type,
                 headerlist=headerlist)
        d.update(kw)
        raise HTTPResponse(self.Response(**d))

# simple utilities

_man = HttpResourceManager()

def isresource(obj):
    try:
        _man.inspectresource(obj)
    except InvalidResource:
        return False
    else:
        return True
    
def isrestful(obj):
    try:
        meth = _man.inspectresource(obj)[-1]
    except InvalidResource:
        return False
    else:
        return meth is not None
    
if __name__ == '__main__':
    from wsgiref.simple_server import make_server
    
    http = HttpResourceManager()

    @http.resource('/issuer/manager')
    def non_restful_html(req):
        yield 'ok'

    @http.resource('/book/(\d\d\d\d)/(\d\d)/(\d\d)')
    def book_plain_GET(req, yyyy, mm, dd):
        yield '%s-%s-%s' % (yyyy, mm, dd)
    
    @http.resource('/c')
    def book_html_POST(req):
        pass

    @http.resource('/d')
    def book_xml_PUT(req):
        pass

    @http.resource('/e')
    def book_plain_DELETE(req):
        http.respond('500 ERR', 'You cannot delete!', 'text/plain')

    h = HttpResourceManager()

    @h.resource('/1')
    def title_html_GET(req):
        return ['1']

    http.add(h, '/a')

    print http._mapping
    
    make_server('', 8080, http).handle_request()
