from bookdb import BookDatabase
bd = BookDatabase("books")
bd.add_from_file("books87.txt")
bd.close()
