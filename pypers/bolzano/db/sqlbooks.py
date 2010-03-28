"""
Add into the books database a books table generated from text files.
"""

def book_from(filename):
    for id, line in enumerate(file(filename)):
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
        yield id + 1, title, score, author, date, genre, nation

def db_from_file(fname):
    print "CREATE DATABASE IF NOT EXISTS books; use books;"
    print """CREATE TABLE IF NOT EXISTS books
    (id SMALLINT(3) NOT NULL, title VARCHAR(30), score CHAR(4),
    author VARCHAR(20), date CHAR(10), genre CHAR(2), nation char(2),
    PRIMARY KEY (id));"""
    print "INSERT INTO books VALUES %s" % ",".join(
        [str(vals) for vals in book_from(fname)])

if __name__ == "__main__":
    db_from_file("books87.txt")
