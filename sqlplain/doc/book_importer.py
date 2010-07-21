import re, csv, itertools
from sqlplain.namedtuple import namedtuple

class BookImporter(object):

    # format of the CSV files to be imported
    BOOKFILE = re.compile('books(\d\d).csv')
    Book = namedtuple('title author genre nation date')

    def __init__(self, db):
        self.db = db
        self.bookid = KTable(db, 'bookid')
        self.book = KTable(db, 'book')
        self.score = dict(KTable(db, 'score'))
        self.genre = KTable(db, 'genre')
        self.nation = KTable(db, 'nation')
        self.counter = itertools.count(1)

    def import_books(self, books):
        for b in books:
            self.bookid.insert_row(
                id = self.counter.next(),
                title = b.title,
                author = b.author,
                rdate = b.date)
            self.book.insert_row(
                id = id,
                genre = b.genre,
                nation = b.nation,
                score = self.scoredic[b.score])

    def import_all(self, dirname=None):
        dirname = dirname or self.db.uri.scriptdir
        for fname in os.listdir(dirname):
            mo = self.BOOKFILE.match(fname)
            f = file(os.path.join(dirname, fname))
            if mo:
                books = map(self.Book, csv.reader(f))
                f.close()
                self.import_books(books)


if __name__ == '__main__':
    db = connect('bookdb')
    imp = BookImporter(db)
    imp.import_all()
