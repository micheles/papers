import MySQLdb

DATAFILE = "/home/micheles/md/varie/libri/libri.txt"

def make_record(line):
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
    return title, score, author, date, genre, nation

def escape(s):
    s = s.replace("'", "\\'")
    s = s.replace('"', '\\"')
    return s
    
def tup2str(tup):
    return "(%s)" % ", ".join(['"%s"' % escape(s) for s in tup])

if __name__ == "__main__":
    # populate the books table of the books database
    cx = MySQLdb.connect()
    cu = cx.cursor()
    cu.execute("create database if not exists books;")
    cu.execute("use books;")
    cu.execute("drop table if exists books;")
    cu.execute("""create table books
    (title varchar(30), score char(4), author varchar(20), read_date date,
    genre char(2), nation char(2));""")
    for line in file(DATAFILE):
        cu.execute("insert into books values %s;" % tup2str(make_record(line)))
