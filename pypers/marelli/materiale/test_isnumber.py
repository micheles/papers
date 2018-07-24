# test_isnumber.py

import unittest

from isnumber import is_number

class TestIsNumber(unittest.TestCase):

    def setUp(self):
        print "sto inizializzando"
        
    # test positivi
    def test_1(self):
        "Testa che '1' è un numero buono."
        self.assertTrue(is_number("1"))
    def test_2(self):
        "Testa che '1.3' è un numero buono."
        self.assertTrue(is_number("1.3"))
    def test_3(self):
        "Testa che '+1.3' è un numero buono."
        self.assertTrue(is_number("+1.3"))
    def test_4(self):
        "Testa che '-1.3' è un numero buono."
        self.assertTrue(is_number("-1.3"))

    # test negativi
    def test_5(self):
        "Testa che '1-.3' non è un numero buono."
        self.assertFalse(is_number("1-.3"))
    def test_6(self):
        "Testa che 'à non è un numero buono."
        self.assertFalse(is_number("a"))
    def test_7(self):
        "Testa che '42' è un numero buono."
        self.assertTrue(is_number("42"))
    
    def tearDown(self):
        print "Sto chiudendo quello che c'è da chiudere"

if __name__ == "__main__":
    unittest.main()


