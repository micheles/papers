import re
import cgi
import inspect
import operator
import traceback
from UserDict import DictMixin
from wsgiref.util import shift_path_info

NAMEDGROUP = re.compile(r'\(\?P<\w+>[^)]+?\)')
        
class HTTPResponse(Exception):
    "Should be instantiated with a Response object as first argument"

class InvalidResource(TypeError):
    "Should be instantiated with a resource object as first argument"

class DuplicateResource(KeyError):
    "Raised when trying to declar twice the same resource"

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

class WrongArguments(TypeError):
    """
    Raised when binding a resource with the wrong number of arguments
    with respect to its declaration.
    """
    @classmethod
    def check(cls, args, expected_args):
        if len(args) != len(expected_args):
            raise cls('Expected %d arguments (%s), got %d' %
                      (len(expected_args), ', '.join(expected_args), len(args)))

class Binding(object):
    """
    A container for a RESTful resource binding.
    """
    # you may tinker with those in subclasses
    valid_content_types = dict(text='text/plain',
                               html='text/html',
                               xml='text/xml',
                               json='application/json')
    valid_methods = ['GET', 'POST', 'PUT', 'DELETE']
    
    @classmethod
    def split_fullname(cls, fullname):
        chunks = fullname.split('_')
        if len(chunks) < 2: # there must be at least one underscore
            raise InvalidResource(obj)
        has_method_suffix = chunks[-1] in cls.valid_methods
        if has_method_suffix and chunks[-2] in cls.valid_content_types:
            basename = '_'.join(chunks[:-2])
            return basename, chunks[-2], chunks[-1]
        elif chunks[-1] in cls.valid_content_types:
            basename = '_'.join(chunks[:-1]) # method not specified
            return basename, chunks[-1], 'ALL'

    def __init__(self, fullname, path, kw):
        self.fullname = fullname        
        self.path = path
        self.kw = kw
        self.basename, self.content_type, self.method =  self.split_fullname(
            fullname)
        self.resource = None
        # parse the path regexp and extracts the expected args
        self.expected_args = []
        for mo in NAMEDGROUP.finditer(path):
            group = mo.group()
            self.expected_args.append(group[4:group.index('>')])
        self.splitlist = NAMEDGROUP.split(path)
        vars(self).update(kw)

    def bind(self, resource):
        "Bind the given resource if its arguments are consistent with the path"
        if inspect.isfunction(resource):
            # strip the first argument, req
            varnames = resource.func_code.co_varnames[1:]
        elif inspect.ismethod(resource):
            # strip the first two arguments, self and req
            varnames = resource.func_code.co_varnames[2:]
        elif hasattr(resource, '__call__'):
            varnames = resource.__call__.func_code.co_varnames[2:]
        WrongArguments.check(varnames, self.expected_args)
        self.resource = resource

    def __repr__(self):
        return '<resource %s %s %s>' % (self.fullname, self.path, self.kw)

class Request(object):
    "A poor man request class. You may want to use webob.Request instead." 
    def __init__(self, environ):
        self.environ = environ
        self.method = environ['REQUEST_METHOD']
        self.remote_user = environ.get('REMOTE_USER')
        self.params = cgi.parse(environ.get('wsgi.input'), environ, 
                                keep_blank_values=True, strict_parsing=False)

class Response(object):
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

def safecall(call_hook, resource, req, args, content_type, show_tb=True):
    """
    Utility calling the resource component and returning a WSGI
    response.  The component can return a sequence of strings or raise
    an HTTPResponse, which is trapped, or an unexpected exception,
    which is trapped if show_tb=True and re-raised otherwise. The only
    way to change the headers for the resource is to raise an
    HTTPResponse by using http.respond.
    """
    try:
        result = call_hook(resource, req, args, content_type, show_tb) 
        if result is None:
            raise InvalidResource(resource, 'returned None')
        elif isinstance(result, str):
            raise InvalidResource(resource, 'returned a string instead of a '
                                  'sequence of strings', result)
        output = ''.join(result) # str or unicode
    except HTTPResponse, exc: # expected exception
        res = exc.args[0]
    except: # unexpected exception
        if show_tb: # show the traceback
            res = Response(traceback.format_exc(), '500 ERR')
        raise # let the server manage the error
    else: # success
        res = Response(output, '200 OK', content_type)
    return res

# in spite of the name, it also dispatches on generic WSGI applications
class HttpResourceDispatcher(object):
    """
    A WSGI dispatcher based on an ordered mapping
    path -> RESTdict[resource | WSGI app].
    You should specify the Request and Response class. 
    If a nontrivial decorator is given, it is applied to the resources.
    If show_tb is True, then broken resources return the full traceback.
    """
    Binding = Binding

    def __init__(self, Request=Request, Response=Response):
        self.Request = Request
        self.Response = Response
        self.show_tb = True
        self._registry = ODict([]) # path, meth -> binding
        self._declared = {} # name, ctype, meth -> binding

    def url_for(self, path_regex, meth="GET", *args):
        "Reconstruct the path by interpolating a path regex"
        binding = self._registry[path_regex, meth]
        WrongArguments.check(args, binding.expected_args)
        # splitlist contains the path split at its arguments
        out = [binding.splitlist[0]] 
        for chunk, arg in zip(binding.splitlist[1:], args):
            out.append(arg)
            out.append(chunk)
        return ''.join(out)

    def dispatch(self, env, start_resp):
        "Dispatch both on resources and sub applications"
        reqmeth = env['REQUEST_METHOD']
        script_name = env.get('SCRIPT_NAME', '')
        path_info = env.get('PATH_INFO', '')
        fullpath = script_name + path_info
        for (path, meth), obj in self._registry.iteritems():
            mo = re.match(path, path_info)
            if mo:
                if not isinstance(obj, self.Binding): # assume WSGI app
                    shift_path_info(env) # subdispatch
                    res = obj
                else: # true resource
                    resource = obj.resource
                    if resource is None:
                        res = Response('Unbound Resource', '500 ERR')
                    elif meth not in ('ALL', reqmeth):
                        res = Response('Method Not Allowed', '405 ERR')
                    else: # call the resource
                        res = safecall(
                            self.call_hook, resource, self.Request(env), 
                            mo.groups(), obj.content_type, path)
                break
        else: # no match, no break
            res = Response('Unknown resource %r' % fullpath, '404 Not Found')
        return res(env, start_resp)

    __call__ = dispatch # make the dispatcher a WSGI app

    def __iter__(self): # flatten functionality can be implemented outside
        for b in self._registry.itervalues():
            if isinstance(b, self.Binding):
                yield b

    def call_hook(self, resource, req, args, ctype, regex):
        "To be overridden, The hook is called everytime you call a resource"
        return resource(req, *args)
                
    def bind_hook(self, resource, path, kw):
        "To be overridden, The hook is called right before binding a resource"
        return resource

    def register_app(self, app, path, meth='ALL'):
        "Register a WSGI application to an URL"
        assert path.startswith('/'), path
        if (path, meth) in self._registry:
            raise DuplicateResource(path)
        self._registry[path, meth] = app

    def resource(self, fullname, path, **kw):
        "Declare a resource with a given name, content_type and path"
        assert path.startswith('/'), path
        b = self.Binding(fullname, path, kw)
        name, ctype, meth = b.basename, b.content_type, b.method
        try:
            decl = self._declared[name, ctype, meth]
        except KeyError: # new binding
            self._registry[path, meth] = b
            self._declared[name, ctype, meth] = b
        else:
            raise DuplicateResource(decl)

    def bindall(self, obj_or_dic):
        "Binds all the declared resources in obj_or_dic"
        if hasattr(obj_or_dic, 'keys'): # is a dict
            getter = obj_or_dic.__getitem__
        else: # is a new-style object
            getter = obj_or_dic.__getattribute__
        for key, declared in self._declared.iteritems():
            try:
                resource = getter('_'.join(key))
            except (KeyError, AttributeError):
                continue
            newres = self.bind_hook(resource, declared.path, declared.kw)
            declared.bind(newres)

    def respond(self, body, status='200 OK', content_type=None, 
                headerlist=None, **kw):
        "Raise a HTTPResponse"
        d = dict(body=body, status=status, content_type=content_type,
                 headerlist=headerlist)
        d.update(kw)
        raise HTTPResponse(self.Response(**d))

# simple utilities for debugging/testing

def access(self, path_info, REQUEST_METHOD='GET', **env):
    "Access the resource specified by the given path_info"
    env.update(PATH_INFO=path_info, REQUEST_METHOD=REQUEST_METHOD)
    statusbox = ['status']
    def set_status(status, headers):
        statusbox[0] = status
    body = ''.join(self(env, set_status))
    return statusbox[0], body
 
def show(self, path_info, REQUEST_METHOD='GET', **env):
    "For use in the interactive interpreter and in doctests"
    status, body = access(path_info, REQUEST_METHOD, **env)
    print status
    print body.replace('\n\n', '\n.\n')

if __name__ == '__main__':

    http = HttpResourceDispatcher()
    resource = http.resource

    resource('non_restful_html', '/issuer/dispatcher')
    resource('book_text_GET', 
             r'/book/(?P<yyyy>\d\d\d\d)/(?P<mm>\d\d)/(?P<dd>\d\d)')
    resource('book_html_POST', '/c')
    resource('book_xml_PUT', '/d')
    resource('book_text_DELETE', '/e')

    class Test(object):
        def non_restful_html(self, req):
            yield 'ok'

        def book_text_GET(self, req, yyyy, mm, dd):
            yield '%s-%s-%s' % (yyyy, mm, dd)

        def book_html_POST(self, req):
            pass

        def book_xml_PUT(self, req):
            pass

        def book_text_DELETE(self, req): ## BUNG!!
            http.respond('500 ERR', 'You cannot delete!', 'text/plain')

    h = HttpResourceDispatcher()
    h.resource('title_html', '/1')

    def title_html_GET(req):
        return ['1']

    h.bindall(dict(title_html_GET=title_html_GET))
    http.register_app(h, '/a')
    http.bindall(Test())

    for binding in http:
        print binding

    print access(http, '/a')
    #http = BasicAuthMiddleware(http, lambda u,p: True)

    #from wsgiref.simple_server import make_server
    #server = make_server('', 8080, http)
    #server.handle_request()
    #server.serve_forever()
