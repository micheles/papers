"""Method to be added to MySite."""

from quixote.form.form import Form
from quixote.form.widget import *
from quixote.html import htmltext
from user_passwd_db import add_user_passwd, valid_user,ErrorAlreadyExistingUser
from quixote import get_user, get_session

def user_passwd_form():
    form = Form()
    form.add(StringWidget, "un", title = "Username")
    form.add(PasswordWidget, "pw", title = "Password")
    form.add(SubmitWidget, "submit", "Submit")
    return form

def register(self):
    form = user_passwd_form()
    if form.is_submitted():
        try:
            add_user_passwd(self.cx.cursor(), form["un"], form["pw"])
        except ErrorAlreadyExistingUser, e:
            return str(e)
        return "You are now registered!"
    else:
        return htmltext("<h1>Registration Form</h1>") + form.render()

def login(self):
    user = get_user()
    if user is None:
        form = user_passwd_form()
        if form.is_submitted():
            if valid_user(self.cx.cursor(), form["un"], form["pw"]):
                get_session().set_user(form["un"]) # this is the point!
                return "You are logged in."
            else:
                return "You are not registered or your password is invalid!"
        else:
            return htmltext("<h1>Login Form</h1>") + form.render()
    else:
        return "You are already logged in!"

def logout(self):
     get_session().set_user(None)
     return "Now you are logged out."

def private(page):
    def private_page(self):
        user = get_user()
        if user:
            return page(self)
        else:
            return "This page can only be accessed by registered members. " \
                   "Please <a href='login'>login</a>."
    private_page.__name__ = page.__name__
    return private_page
