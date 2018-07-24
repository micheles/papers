import cherrypy as cp

class Root(object):
    @cp.expose
    def index(self):
        return 'This is the index page'

if __name__ == '__main__':
    cp.root = Root()
    cp.server.start()
