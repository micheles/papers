# split.py
import re

def outer():
    def split(text, sep = re.compile(r"\s*[,;]\s*")):
        """Split a string taking as separators "," ";".
        Example:
        >>> from split import split
        >>> split("hello, world!; welcome to the Italian Code Jam!")
        ['hello', 'world!', 'welcome to the Italian Code Jam!']
        """
        return sep.split(text)

if __name__ == "__main__":
    import __main__, doctest
    doctest.testmod(__main__)
