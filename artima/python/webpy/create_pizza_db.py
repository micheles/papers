import os
import web

DBFILE = '/tmp/pizza.sqlite' # assumes a Unix-like system

CREATE_EMPLOYEE = '''\
CREATE TABLE employee (
    login VARCHAR(16) PRIMARY KEY,
    pwd VARCHAR(16))'''

EMPLOYEES = [('employee%d' % i, None) for i in range(1, 21)]

CREATE_PIZZA = '''\
CREATE TABLE pizza (
    employee VARCHAR(16) PRIMARY KEY REFERENCES employee(login),
    pizza VARCHAR(128),
    drink VARCHAR(64))'''

def create_pizza_db(db):
    "Create the test db as an SQLite file"
    if os.path.exists(db):
        os.remove(db) # start from a fresh db
    db = web.database(dbn='sqlite', db=db)
    
    db.query(CREATE_EMPLOYEE)
    db.query(CREATE_PIZZA)
    db.multiple_insert( # populate employee
        'employee', [dict(login=login, pwd=pwd)
                     for login, pwd in EMPLOYEES],
        seqname=False)
    db.multiple_insert( # populate pizzas
        'pizza', [dict(employee='employee2', pizza='marinara', drink='coca'),
                  dict(employee='employee3', pizza='prosciutto', drink='fanta'),
                  dict(employee='employee4', pizza='diavola', drink='chinotto'),
                  ], seqname=False)

if __name__ == '__main__':
    db = create_pizza_db(DBFILE)
