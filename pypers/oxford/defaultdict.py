
def defaultdict(default, dictclass=dict):
    class defdict(dictclass):
        def __getitem__(self, key):
            try:
                return super(defdict, self).__getitem__(key)
            except KeyError:
                return self.setdefault(key, default)
    return defdict

d = defaultdict(0)()
d["x"] += 1
d["x"] += 1
d["y"] += 1
print d

d = defaultdict(list())()
d["x"].append(1)
d["x"].append(2)
d["y"].append(1)
print d
