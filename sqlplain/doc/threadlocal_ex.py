import threading
from sqlplain import lazyconnect, do
from sqlplain.util import create_db

create_book = do('''
CREATE TABLE book(
  title VARCHAR(64),
  author VARCHAR(32),
  rdate DATE)
''')

select_author = do('''
SELECT author FROM book
''')

insert_author = do('''
INSERT INTO book VALUES ('No title', ?, '2008-11-22')
''')

def show_authors(db):
    thread = threading.currentThread().getName()
    print('Printing from %s' % thread)
    insert_author(db, thread)
    for i, author in enumerate(select_author(db)):
        print(i, author[0])
    print('---------------------')
    
def run_many(N, action, db):
    threads = [threading.Thread(None, lambda : action(db)) for _ in range(N)]
    try:
        for th in threads:
            th.start()
    finally:
        th.join()
    
if __name__ == '__main__':
    db = create_db('sqlite:///tmp.sqlite', force=True, threadlocal=True)
    create_book(db)
    run_many(10, show_authors, db)
