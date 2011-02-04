"""
Example of usage:

from ms.quixote_utils24 import SimpleDirectory, Publish
from quixote.server import twisted_server

class Root(SimpleDirectory):
    def _q_index(self):
        return "hello"

Publish(Root(), showpage="", server=twisted_server)
"""
import os, sys, threading, webbrowser
from quixote.directory import Directory
from quixote.session import Session, SessionManager
from quixote.server import simple_server
from quixote.errors import AccessError
from quixote.form.form import Form
from quixote.form.widget import StringWidget, PasswordWidget, SubmitWidget
from quixote import get_response, get_user, get_session
from quixote.publish import Publisher
from quixote.html import href, htmltext

################## PUBLICATION MACHINERY ###########################

elinks = webbrowser.GenericBrowser('xterm -e elinks %s')
lynx = webbrowser.GenericBrowser('xterm -e lynx -accept_all_cookies %s') 
webbrowser.register("elinks", webbrowser.GenericBrowser, elinks)
webbrowser.register("lynx", webbrowser.GenericBrowser, lynx) # second choice
webbrowser.register("konqueror", webbrowser.Konqueror)

class Publish(object):
    def __init__(self, root, Publisher=Publisher, Session=Session,
                 session_mapping=None, server=simple_server,
                 showpage=None, browser="mozilla", host='', port=7080):
        self.root = root
        self.Publisher = Publisher
        self.Session = Session
        self.session_mapping = session_mapping
        self.server = server
        self.host = host
        self.port = port
        self.showpage = showpage
        self.browser = browser
        if showpage is not None: # wait a bit and then open a browser
            threading.Timer(0.1, self.show).start()
        self.publish()
            
    def make_publisher(self):
        return self.Publisher(self.root, session_manager=SessionManager(
            self.Session, self.session_mapping))
                
    def publish(self):
        try:
            self.server.run(self.make_publisher, self.host, self.port)
        except KeyboardInterrupt:
            print "Server stopped by CTRL-C."

    def show(self):
        webbrowser.get(self.browser).open("http://localhost:%s/%s" % (
            self.port, self.showpage))

def public(f):
    f._q_exported = True   
    return f

class AutoExport(type):
    """Attributes of instances of AutoExport with a "_q_exported"
    flag are automatically added to the class _q_exports list.
    """
    def __init__(cls, name, bases, dic):
        cls._q_exports = sum(
            (getattr(base, "_q_exports", []) for base in bases), []) or [""] 
        for k in dic:
            setattr(cls, k, dic[k])
        super(AutoExport, cls).__init__(name, bases, dic)
    def __setattr__(cls, name, value):
        if hasattr(value, "_q_exported"):
            cls._q_exports.append(name)
        super(AutoExport, cls).__setattr__(name, value)

class SimpleDirectory(Directory):
    __metaclass__ = AutoExport

    def _q_index(self): # to be overridden in subclasses
        return "Welcome to the root of your application."

################# AUTHENTICATION MACHINERY #####################

class User(object):
    def __init__(self, username=None, password=None):
        self.username = username
        self.password = password
    def __str__(self):
        return "<User: %s %s>" % (self.username, self.password)
    def permissions(self):
        """Returns the list of methods starting with 'can_'."""
        return [perm for perm in dir(self) if perm.startswith("can_")]
     
class private(object):
    """Redirect to the login page if the user is not logged in or if he does
    not have the right permissions."""
    # obviously, this assumes a login page exists and is called 'login'
    msg = """\
You are trying to access a page restricted to %r. 
Please <a href='login'>login</a> as a valid user.""" 
    def __init__(self, *groups_with_access):
        self.valid_groups = groups_with_access or (User,)
    def __call__(self, method):
        def wrapper(root):
            user = get_user()
            if not user or not isinstance(user, self.valid_groups):
                root.resume = meth_name
                valid_groups = ", ".join(
                    cls.__name__ for cls in self.valid_groups)
                return  self.msg % valid_groups 
            else:
                return method(root)
        meth_name = method.func_name
        wrapper.func_name = meth_name
        wrapper._q_exported = True
        return wrapper

import shelve

def un_pw_form():
    form = Form()
    form.add(StringWidget, 'username', title="Username")
    form.add(PasswordWidget, 'password', title="Password")
    form.add(SubmitWidget, "submit", "Submit")
    return form
    
class WebDirectory(SimpleDirectory):
    """A simple directory plus a login form with a resume capability."""
    resume = None
    registered_users = shelve.open("registered_users")
        
    @public
    def mainpage(self):
        if not get_user():
            self.resume = 'mainpage'
            return """Welcome to the best site of the World! If this is
            your first visit, please <a href='register'>register</a> else
            <a href='login'>login</a>."""
        else:
            return htmltext(
                "Content of the site."
                "Click <a href='logout'>here</a> to logout.")            

    _q_index = mainpage
    
    @public
    def register(self):
        form = un_pw_form()
        if not form.is_submitted():
            return htmltext("<h1>Registration form</h1>") + form.render()
        else:
            self.registered_users[form["username"]] = form["password"]
            self.registered_users.sync()
            return htmltext("Thank you for registering. ") + self.login()
        
    @public
    def login(self, User=User):
        """Subclassess are free to override the login form. This is an example
        of how to do it:

        class MyWebDirectory(WebDirectory):
            @public
            def login(self, User=MyUserClass):
                return super(MyWebDirectory, self).login(MyUserClass)
        """
        form = un_pw_form()
        if not form.is_submitted():
            return htmltext("<h1>Login form</h1>") + form.render()
        un, pw = form["username"], form["password"]
        if un not in self.registered_users:
            return """\
        You are not a registered user.
        Please <a href='register'>register</a> first."""
        elif pw != self.registered_users[un]:
            return "Wrong password. Please <a href='login'>retry</a>."""
        
        user = User(un, pw)
        get_session().set_user(user)
        msg = "Now you are logged in as %r. " % user.username
        if self.resume is not None:
            msg += htmltext(
            "You can <a href=%r>resume</a> from where you left." % self.resume)
        return msg

    @private()
    def logout(self):
        get_session().set_user(None)
        return "You are now logged out."
    
########################### NOT ESSENTIAL STUFF ############################

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

################## UI STUFF ##################################

from ms.iter_utils import Cycle, chop

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
            r = " ".join("<th>%s</th>" % col for col in row)
        else:
            r = " ".join("<td>%s</td>" % col for col in row)
        return "<tr>%s</tr>" % r

    def maketable(self):
        #yield "<div align='center'>"
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
        #yield "</div>"

    def render(self):
        return htmltext("\n").join(map(htmltext, self.maketable()))
