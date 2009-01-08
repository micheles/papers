from paste.urlparser import StaticURLParser
from wsgiref.util import shift_path_info
from paste.httpserver import serve

# the template of the page
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
'''

# the body of the page
body = "<h3>click me and I will disappear!</h3>"

# the javascript relying on JQuery
js = """
$("h3").click(function(event){
   $(this).hide("slow");
 });
"""

static = StaticURLParser(directory='/tmp')

def application(env, resp):
    """Return the JQuery-enhanced HTML page  and dispatch on the 
    static directory too"""
    name = shift_path_info(env)
    if name == 'static':
        return static(env, resp)
    resp('200 OK', [('Content-type', 'text/html')])
    return [html % (js, body)]

if __name__ == '__main__':
    serve(application, '', 8000)
