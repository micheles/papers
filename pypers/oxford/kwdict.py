# kwdict.py

class kwdict(dict): # or UserDict, to make it to work with Zope
    """A typing shortcut used in place of a keyword dictionary."""
    def __getattr__(self, name):
        return self[name]
    def __setattr__(self, name, value):
        self[name] = value


