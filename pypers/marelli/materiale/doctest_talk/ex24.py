"""
>>> print "Hello, World!"
Hello, World!

>>> print "Hello World! 2" #doctest: +ELLIPSIS
Hello ... 2
>>> print range(1000) #doctest: +ELLIPSIS
[0, 1, 2, ..., 999]

>>> print "ciao come va nina?" #doctest: +ELLIPSIS
ciao ... nina?

"""

if __name__ == "__main__":
    import doctest, __main__
    doctest.testmod(__main__)
