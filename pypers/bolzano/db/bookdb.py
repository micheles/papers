import shelve

class Book(object):
    FIELDS = ["title", "score", "author", "date", "genre", "nation"]
    def __init__(self, title, score, author, date, genre, nation, dbkey):
        self.title = title
        self.score = score
        self.author = author
        self.date = date
        self.genre = genre
        self.nation = nation
        self.dbkey = dbkey
    def __str__(self):
        return """<%(dbkey)s %(title)s %(score)s %(author)s %(date)s
%(genre)s %(nation)s>""" % vars(self)

class BookDatabase(object):
    def __init__(self, datafile):
        self.db = shelve.open(datafile)
        self.recno = len(self.dbkeys())
        self.print_all()
    def commit(self):
        self.db.sync()
    def close(self):
        self.db.close()
    def getkey(self):
        return "%06d" % self.recno
    def dbkeys(self):
        dbkeys = self.db.keys()
        dbkeys.sort()
        return dbkeys    
    def add_book(self, title, score, author, date, genre, nation):
        self.db[self.getkey()] = Book(
            title, score, author, date, genre, nation, self.getkey())
        self.recno += 1
    def edit_book(self, dbkey, title, score, author, date, genre, nation):
        self.db[dbkey] = Book(
            title, score, author, date, genre, nation, dbkey)
    def add_from_file(self, filename):
        for line in file(filename):
            title = line[0:33].strip()
            score = line[33:38].strip()
            author = line[38:51].strip()
            dd, mm, aa = line[51:61].strip().split("-")
            genre = line[61:65].strip()
            nation = line[65:67]
            if "87" <= aa <= "99":
                aaaa = "19" + aa
            else:
                aaaa = "20" + aa
            date = "-".join([aaaa, mm, dd])
            self.add_book(title, score, author, date, genre, nation)
    def del_book(self, book):
        del self.db[book.dbkey]
    def get_book(self, dbkey):
        return self.db[dbkey]
    def print_all(self):
        for key in self.dbkeys():
            print self.db[key]

if __name__ == "__main__":
    bd = BookDatabase("books")
   
