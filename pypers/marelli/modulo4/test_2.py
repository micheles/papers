"""
Generative tests: generators returning check functions and their parameters
"""
def isequal(x, y):
    assert x == y
    
def test_gen():
    for i, j in (1, 1), (2, 3), (4, 5):
        yield isequal, i, j
        
