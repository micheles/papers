# split.py
import re
SEP = re.compile(r"\s*[,;]\s*")
 
def split(text):
    """Split a string taking as separators "," ";".
    Example:
    >>> from split import split
    >>> split("hello, world!; welcome to PyUK!")
    ['hello', 'world!', 'welcome to PyUK!']
    """
    return SEP.split(text)

if __name__ == "__main__":
    import doctest; doctest.testmod()
