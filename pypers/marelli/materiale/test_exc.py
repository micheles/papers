# test_exc.py

import unittest

def divide(a, b):
    return a/b

class TestIsNumber(unittest.TestCase):
    def test_1(self):
        "Divide 4/2"
        self.assertEqual(divide(4,2), 2)
    def test_2(self):
        "Divide 4/0"
        self.assertRaises(ZeroDivisionError, divide, 4, 0)


if __name__ == "__main__":
    unittest.main()


