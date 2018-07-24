import web
from resource import Register
from create_pizza_db import DBFILE
from simplejson import dumps

resource = Register()

db = web.database(dbn='sqlite', db=DBFILE)

@resource('/pizzas')
def pizzas_GET(input):
    res = db.select('pizza', order='pizza')
    return dumps(res.list())

@resource('/pizza/([a-z\d]+)')
def pizza_GET(input, employee):
    res = db.select('pizza', locals(), where="employee=$employee")
    return dumps(res.list())

@resource('/pizza/([a-z\d]+)')
def pizza_POST(input, employee):
    db.insert('pizza', employee=employee, pizza=input.pizza,
              drink=input.drink, seqname=False)

@resource('/pizza/([a-z\d]+)')
def pizza_PUT(input, employee):
    count = db.update('pizza', 'employee=$employee', vars=locals(),
                    pizza=input.pizza, drink=input.drink)
    return str(count)

@resource('/pizza/([a-z\d]+)')
def pizza_DELETE(input, employee):
    count = db.delete('pizza', 'employee=$employee', vars=locals())
    return str(count)

if __name__ == '__main__':
    resource.app.run()
