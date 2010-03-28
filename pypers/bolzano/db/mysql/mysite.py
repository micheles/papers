from quixote_utils import RootDirectory, get_user
from quixote.publish import SessionPublisher
from registration import register, login, logout, private
import MySQLdb

class MySite(RootDirectory):
    _q_exports = ["register", "login", "logout", "mainpage", "hello"]
    def __init__(self):
        self.cx = MySQLdb.connect(db="site_db")
    register = register
    login = login
    logout = logout

    @private
    def hello(self):
        return "hello"
    
if __name__ == "__main__":
    site = MySite()
    site.set_default(Publisher=SessionPublisher)
    site.publish_show("hello")
