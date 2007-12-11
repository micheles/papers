import os, cgi, traceback
from wsgiref import simple_server
from tempfile import mkstemp
from simpleplotter import make_graph

def getformdict(env):
    qs = env.get('QUERY_STRING')
    if qs:
        return dict((k, v[0]) for k, v in cgi.parse_qs(qs).iteritems())

def app(env, resp):
    form = getformdict(env)
    if form and form.get('submitted'):
        try:
            fname = make_graph(form.get('code'), batch=True)
        except Exception, e:
            resp('500 ERR', [('Content-type', 'text/plain')])
            return [traceback.format_exc()]
        else:
            resp('200 OK', [('Content-type', 'image/png')])
            return file(fname)
    else:
        resp('200 OK', [('Content-type', 'text/html')])
        return [
            'Try values such as <pre>fri-gb;AVE</pre>',
            '<pre>fri-gb;TSCO</pre> <pre>fri-us;DELL</pre>',
            '<form>', 'insert code ',
            '<input type="text" name="code"/>',
            '<input type="submit", name="submitted", value="submit" />',
            '</form>']
    
if __name__ == '__main__':
    #from paste.auth.basic import AuthBasicHandler
    #app = AuthBasicHandler(
    #    app, 'plotter realm', lambda e, u, p: u=='pippo' and p=='lippo')
    simple_server.make_server('', 8000, app).serve_forever()
