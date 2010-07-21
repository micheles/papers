def attributoTraducibile(**dic):
    def get(self):
        return dic[self.lingua]
    def set(self, traduzione):
        dic[self.lingua]= traduzione
    return property(get, set)

class Oggetto(object):
    definizione = attributoTraducibile(it="vaso", en="potter")
    tipologia = attributoTraducibile(it="antico", en="ancient")

o = Oggetto()
o.lingua = "it"
print o.definizione
o.lingua = "en"
print o.definizione

o.lingua = "it"
o.definizione = "Vaso"
print o.definizione
o.lingua = "en"
o.definizione = "Potter"
print o.definizione 
