from jquery_helper import Dispatcher
from paste.httpserver import serve

js = '$("h3").click(function(event){ $(this).hide("slow"); });'
root = ('<h3>Root Page</h3><a href="/other-page">Go to some other page</a>', js)
page = ('<h3>Other page</h3><a href="/">Go back to the root page</a>', js)

if __name__ == '__main__':
    app = Dispatcher('/tmp', root)
    app.add('other-page', page)
    serve(app, '', 8000)
