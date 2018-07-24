import os, sys, time, webbrowser
from quixote.directory import Directory
from quixote.publish import Publisher, SessionPublisher
from quixote.session import Session, SessionManager
from quixote.server import simple_server
from quixote.errors import AccessError
from quixote import get_response, get_user
from quixote.html import href, htmltext
from quixote.form.form import Form
from quixote.form.widget import *

webbrowser.register("konqueror", webbrowser.Konqueror) 
elinks = webbrowser.GenericBrowser('xterm -e elinks %s')
lynx = webbrowser.GenericBrowser('xterm -e lynx -accept_all_cookies %s') 
webbrowser.register("elinks", webbrowser.GenericBrowser, elinks)
webbrowser.register("lynx", webbrowser.GenericBrowser, lynx) # second choice

class RecognizeExports(type):
    def __init__(cls, name, bases, dic):
        super(RecognizeExports, cls).__init__(cls, name, bases, dic)
        for k in dic: setattr(cls, k, dic[k])
    def __setattr__(cls, name, value):
        if hasattr(value, "_q_exported"):
            cls._q_exports.append(name)
        super(RecognizeExports, cls).__setattr__(name, value)

# by definition, the root directory is a singleton
class RootDirectory(Directory):
    _q_exports = [""]
    __metaclass__ = RecognizeExports
    __Publisher = Publisher
    __server = simple_server
    __port = 7080
    
    def _q_index(self):
        return "Welcome to the root of your application."

    def set_default(self, server=simple_server, Publisher=Publisher,
                    Session=Session, session_mapping=None, port=7080):
        self.__server = server
        self.__Publisher = Publisher
        self.__Session = Session
        self.__session_mapping = session_mapping 
        self.__port = port

    __init__ = set_default

    def publish(self):
        if issubclass(self.__Publisher, SessionPublisher):
            create_pub = lambda : self.__Publisher(
                self, SessionManager(self.__Session, self.__session_mapping))
        else:
            create_pub = lambda : self.__Publisher(self)
        self.__server.run(create_pub, '', self.__port)

    def publish_show(self, page="", browser="mozilla"):
        if os.fork(): # parent
            self.publish()
        else: # child
            webbrowser.get(browser).open(
                "http://localhost:%s/%s" % (self.__port, page))

            
class UnauthorizedError(AccessError):
  """The request requires user authentication.

  This subclass of AccessError sends a 401 instead of a 403,
  hinting that the client should try again with authentication.
  """
  status_code = 401
  title = "Unauthorized"
  description = "You are not authorized to access this resource."

  def __init__(self, realm='Protected', public_msg=None, private_msg=None):
      self.realm = realm
      AccessError.__init__(self, public_msg, private_msg)

  def format(self):
      get_response().set_header(
          'WWW-Authenticate', 'Basic realm="%s"' % self.realm)
      return AccessError.format(self)


class User(object):
    def __init__(self, username, password):
        self.username = username
        self.password = password
    def __str__(self):
        return "<User: %s %s>" % (self.username, self.password)
    def permissions(self):
        """Returns the list of methods starting with 'can_'."""
        return [perm for perm in dir(self) if perm.startswith("can_")]
     
def public(f):
    f._q_exported = True   
    return f

class private(object):
    """Redirect to the login page if the user is not logged in or if he does
    not have the right permissions."""
    # obviously, this assumes a login page exists and is called 'login'
    def __init__(self, *groups_with_access):
        self.valid_groups = groups_with_access or (User,)
    def __call__(self, method):
        def wrapper(root):
            user = get_user()
            if not user or not isinstance(user, self.valid_groups):
                root.resume = meth_name
                valid_groups = ", ".join([
                    cls.__name__ for cls in self.valid_groups])
                return "You are trying to access a page restricted to %s. " % \
                       valid_groups + href(
                    "login", "Please login as a valid user.")
            else:
                return method(root)
        meth_name = method.func_name
        wrapper.func_name = meth_name
        wrapper._q_exported = True
        return wrapper

######################## deprecated ############################
    
def old_public(f):
    """Append f.__name__ to the caller's _q_exports. If the caller has
    no _q_exports, creates it."""
    _q_exports = sys._getframe(1).f_locals.setdefault("_q_exports",[""])
    _q_exports.append(f.__name__)
    return f

class old_private(object):
    """Redirect to the login page if the user is not logged in or if he does
    not have the right permissions."""
    # obviously, this assumes a login page exists and is called 'login'
    def __init__(self, *groups_with_access):
        self.valid_groups = groups_with_access or (User,)
    def __call__(self, method):
        def wrapper(root):
            user = get_user()
            if not user or not isinstance(user, self.valid_groups):
                root.resume = meth_name
                valid_groups = ", ".join([cls.__name__ for cls
                                          in self.valid_groups])
                return "You are trying to access a page restricted to %s. " % \
                       valid_groups + href("login", "Please login as a valid user.")
            else:
                return method(root)
        meth_name = method.func_name
        _q_exports = sys._getframe(1).f_locals.setdefault("_q_exports",[""])
        _q_exports.append(meth_name)
        wrapper.func_name = meth_name
        return wrapper

from iter_utils import Cycle, chop

class MultipageTable(object):
    # use Quixote forms
    def __init__(self, body, header=[], maxsize=20):
        self.header = header
        self.maxsize = maxsize
        self.section = Cycle(chop(body, maxsize))
        self.sect = self.section[0] # default
        self.ismultipage = len(self.section) > 1

    def makerow(self, row, header=False):
        if header:
            r = " ".join(["<th>%s</th>" % col for col in row])
        else:
            r = " ".join(["<td>%s</td>" % col for col in row])
        return "<tr>%s</tr>" % r

    def maketable(self):
        if self.ismultipage:
            form = Form()
            form.add(SubmitWidget, "prev", "Prev")
            form.add(SubmitWidget, "next", "Next")
            if form["next"]: # is submitted
                self.sect = self.section.next()
            if form["prev"]: # is submitted
                self.sect = self.section.prev()
            yield "Page #%s of %s" % (self.section.index+1, len(self.section))
        yield "<table border='1'>"
        if self.header:
            yield self.makerow(self.header)
        for row in self.sect:
            yield self.makerow(row)
        yield "</table>"
        if self.ismultipage:
            yield form.render()

    def render(self):
        return htmltext("\n").join(map(htmltext, self.maketable()))
