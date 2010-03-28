r"""
>>> print "foo\n\nbar\n"
foo
<BLANKLINE>
bar
<BLANKLINE>

>>> print range(1000) #doctest: +ELLIPSIS
[0, 1, 2, ..., 999]
          
>>> print range(20) #doctest: +NORMALIZE_WHITESPACE
[0,  1,  2, 3,         4,  5,  6, 7, 8, 9, 10, 11, 
12, 13, 14,  15, 16,  17, 18,  19]
          
"""

if __name__ == "__main__":
    import doctest, __main__
    doctest.testmod(__main__)
