# XMLtag.py

def makeattr(dict_or_list_of_pairs):
    dic = dict(dict_or_list_of_pairs) 
    return " ".join('%s="%s"' % (k, dic[k]) for k in dic) # simplistic

class XMLTag(object):
    def __getattr__(self, name):
        def tag(value, **attr):
            """value can be a string or a sequence of strings."""
            if hasattr(value, "__iter__"): # is iterable
                value = " ".join(value)
            return "<%s %s>%s</%s>" % (name, makeattr(attr), value, name)
        return tag

class XMLShortTag(object):
    def __getattr__(self, name):
        def tag(**attr):
            return "<%s %s />" % (name, makeattr(attr))
        return tag

tag = XMLTag()
tg = XMLShortTag()


