"""Interface to a Username/Password database implemented via MySQL."""

import MySQLdb

class ErrorAlreadyExistingUser(Exception):
    pass

def create_users_db():
    cx = MySQLdb.connect()
    cu = cx.cursor()
    cu.execute("create database if not exists site_db;")
    cu.execute("use site_db;")
    cu.execute("""create table if not exists site_users
    (username varchar(10), password varchar(10));""")
    return cx

def valid_user(cu, username, password):
    return cu.execute("""select * from site_users where username=%r and
    password=%r;""" % (username, password))

def already_exists(cu, username):
    return cu.execute("select * from site_users where username=%r;" % username)

def add_user_passwd(cu, username, password):
    if already_exists(cu, username):
        raise ErrorAlreadyExistingUser("This username is already taken!")
    cu.execute("insert into site_users values (%r, %r);" % (
        username, password))
    
    
if __name__ == "__main__":
    cx = create_users_db()
    cu = cx.cursor()
    add_user_passwd(cu, "simona", "simona")
    cx.close()
    
