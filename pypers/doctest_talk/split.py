# split.py
import re
SEP = re.compile(r"\s*[,;]\s*")
 
def split(text):
    """Split a string taking as separators "," ";".
    Example:
    >>> from split import split
    >>> split("hello, world!; welcome to the Italian Code Jam!")
    ['hello', 'world!', 'welcome to the Italian Code Jam!']
    """
    return SEP.split(text)

if __name__ == "__main__":
    import __main__, doctest
    doctest.testmod(__main__)
