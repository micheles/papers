'''
JQuery is a powerful and easy to use Javascript library.
Paste is a powerful and easy to use Python library for Web programming.
Once in a while I need use both but I always forget the details of
their integration. This post is a note to self to remember what I did
the next time I am going to use both libraries together.

The quick and dirty way
--------------------------------------------------------

Here I will just discuss the quicker approach, suitable for prototyping
applications, not for production use. One can just perform the following
steps:

1. easy install Paste;

2. download the jquery.pack library and save it somewhere, let's say in the /tmp directory;

3. write the body of the web page you want to enhance with javascript, and save in the /tmp;

4. write the javascript code, and save it in /tmp;

5.write a WSGI application serving the directory where JQuery is installed by using ``paste.urlparser.StaticURLParser``.

In practice however, the interesting is to generate the HTML code
and the Javascript code from Python and to treat the generated
page as a WSGI application. Here is an simple script doing exactly
that:

$$jquery_ex

The only nontrivial bit is the ``shift_path_info`` function, which is
part of the standard library starting from Python 2.5 (in ``wsgiref.util``).
``shift_path_info`` extracts the name of the application from the start
of the URL: in particular, if the URL starts with ``/static``, the
application dispatches to the underlying ``StaticURLParser``
application, otherwise the JQuery-enhanced HTML page specified
by ``body`` and ``js`` is returned. Notice that the source code for the 
JQuery library (in compact format) can be retrieved.since the HTML template
contains the line ``script type="text/javascript" src="/static/jquery.pack.js">
</script>``.

A simple page dispatcher
----------------------------------------------------------------

I wrote a little ``Dispatcher`` object to prototype multipage applications.
It uses the same idea of the example before, but it more elegant.
You instantiate the dispatcher by passing to it the path of the directory
containing the static files and a root WSGI application; you can then
add other applications via the ``.add`` method. For sake of convenience,
you can also pass a pair ``(html-body, javascript-code)`` instead of
a regular application, and the pair will be automagically converted into a WSGI
application.

Here is the source code

$$Dispatcher

and here is an example of use:

$$jquery_ex2

If you click on the title, it will slowly disappear.
'''

import jquery_ex, jquery_ex2
from jquery_helper import Dispatcher
from paste.httpserver import serve

if __name__ == '__main__':
    app = Dispatcher('/tmp', (body, js))
    serve(app, '', 8000)
