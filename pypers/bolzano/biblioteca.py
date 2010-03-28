class Libro(object):
    def __init__(self, titolo, autore):
        self.titolo = titolo
        self.autore = autore
    def __str__(self):
        return "%s | %s" % (self. titolo, self.autore)

class Biblioteca(object):
    def __init__(self, libri):
        self.libri = libri
    def ordina_per(self, campo):
        def compara(l1, l2):
            return cmp(getattr(l1, campo), getattr(l2, campo))
        lista = self.libri[:]
        lista.sort(compara)
        return lista

l1 = Libro("Il Signore degli Anelli", "Tolkien")
l2 = Libro("La Luna e' una severa maestra", "Heinlein")

archivio = Biblioteca([l1, l2])

for libro in archivio.ordina_per("titolo"):
    print libro
    
for libro in archivio.ordina_per("autore"):
    print libro
