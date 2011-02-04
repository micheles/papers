"""
An authorization library for resources. It requires an user table with 
fields username, password and role.
"""

from dispatcher import HttpResourceDispatcher, Request, Response

class AuthenticatedUser(object):
    def __init__(self, username):
        self.username = username
    def __str__(self):
        return '<%s %s>' % (self.__class__.__name__, self.username)

def _subclasses(base, acc):
    for sub in base.__subclasses__():
        acc.append(sub)
        _subclasses(sub, acc)

def subclasses(base):
    acc = [base]
    _subclasses(base, acc)
    return acc

def getclass(role):
    "The role is a name of a user class"
    for c in subclasses(AuthenticatedUser):
        if c.__name__ == role:
            return c
    raise NameError('Unknown role %s' % role)

class AuthorizationModel(object):
    """
    Provides a single method getrole extracting the role string
    from the user table. Requires in input as fieldmapping a dictionary
    with keys user, username, password, role; moreover, you can pass
    the placeholder used by the paramstyle of the database driver. 
    """
    def __init__(self, conn, fieldmapping=None, placeholder='?'):
        fieldmapping = fieldmapping or {}
        self.fieldmapping = fieldmapping
        self.placeholder = placeholder
        self.user = fieldmapping.get('user', 'user')
        self.username = fieldmapping.get('username', 'username')
        self.password = fieldmapping.get('password', 'password')
        self.role = fieldmapping.get('role', 'role')
        getrole = 'SELECT %(role)s FROM %(user)s WHERE %(username)s=?'
        self.getrole_templ = (getrole % vars(self)).replace('?', placeholder)
        self.conn = conn
        self.curs = conn.cursor()

    def getuser(self, username):
        "From username to user object"
        # username is authenticated, therefore in the database for sure
        self.curs.execute(self.getrole_templ, (username,))
        role = self.curs.fetchall()[0][0]
        return getclass(role)(username)

class HttpDispatcherAuth(HttpResourceDispatcher):
    """
    A resource manage which looks to the resource table for the given
    (path, meth) and responds unauthorized if the current user role
    is not sufficient to access the resource. Notice that it MUST
    be wrapped in an authentication middleware. getuser is callable
    taking an username and returning an user object.
    """   
    def __init__(self, getuser, Request=Request, Response=Response):
        HttpResourceDispatcher.__init__(self, Request, Response)
        self.getuser = getuser
        self.rolemap = {} # {path->userclass}

    def bind_hook(self, resource, path, kw):
        self.rolemap[path] = kw['roles'] # this is a class
        return resource

    def call_hook(self, resource, req, args, ctype, path):
        auth_classes = self.rolemap[path]
        user = self.getuser(req.remote_user)
        if isinstance(user, auth_classes):
            return HttpResourceDispatcher.call_hook(
                self, resource, req, args, ctype, path)
        else:
            self.respond('User %s has no access permission' % user,
                         '401 Unauthorized')
