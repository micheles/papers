from sqlplain import do

get_authors = do('SELECT author FROM book')
get_titles = do('SELECT title FROM book WHERE author=?')
set_uppercase_titles = do('''
UPDATE book SET title=upper(title)
WHERE author like ? AND pubdate=?
''')
