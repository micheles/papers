from web import Storage
from pizzaserver import *
from create_pizza_db import create_pizza_db, DBFILE

def setup():
    create_pizza_db(DBFILE)

def test_pizzas_GET():
    '[{"employee": "employee4", "drink": "chinotto", "pizza": "diavola"}, {"employee": "employee2", "drink": "coca", "pizza": "marinara"}, {"employee": "employee3", "drink": "fanta", "pizza": "prosciutto"}]' == pizzas_GET(Storage())

def test_pizza_GET():
    assert '[{"employee": "employee2", "drink": "coca", "pizza": "marinara"}]'\
           == pizza_GET(Storage(), 'employee2')

def test_pizza_POST():
    input = Storage()
    input.pizza = 'margherita'
    input.drink = 'sprite'
    assert None is pizza_POST(input, 'employee1')

def test_pizza_PUT():
    input = Storage()
    input.pizza = 'capricciosa'
    input.drink = 'coca'
    assert '1' == pizza_PUT(input, 'employee1')

def test_pizza_DELETE():
    assert '1' == pizza_DELETE(Storage(), 'employee1')
