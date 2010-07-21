class Book(object):
    title_span = 0, 33
    rate_span = 33, 38
    author_span = 38, 51
    date_span = 51, 61
    genre_span = 61, 65
    nation_span = 65, 67
    
    def __init__(self, title, rate, author, date, genre, nation):
        self.title = title
        self.rate = rate
        self.author = author
        self.date = date
        self.genre = genre
        self.nation = nation

    def __repr__(self):
        return self.title + self.rate + self.author + self.date + \
               self.genre + self.nation

class Library(object):
    def __init__(self):
        self.books = []
    def read(self, fname):
        for line in file(fname):
            title = line.__getslice__(*Book.title_span)
            rate = line.__getslice__(*Book.rate_span)
            author = line.__getslice__(*Book.author_span)
            date = line.__getslice__(*Book.date_span)
            genre = line.__getslice__(*Book.genre_span)
            nation = line.__getslice__(*Book.nation_span)
            self.books.append(Book(title, rate, author, date, genre, nation))

lib = Library()
lib.read("books87.txt")

