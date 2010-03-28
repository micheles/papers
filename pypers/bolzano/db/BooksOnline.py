from quixote_utils import RootDirectory
from quixote.form.form import Form
from quixote.form.widget import *
from bookdb import Book, BookDatabase

class DBInterface(RootDirectory):
    _q_exports = ["", "show", "add_book", "edit_book"]
    def __init__(self, db):
        super(DBInterface, self).__init__()
        self.db = db
        self.currentbook = self.db.get_book("000000")
    def _q_index(self):
        return """Welcome to our online library! <br/><br/>
        Please go to our <a href="show">show</a> page."""
    def show(self):
        return "list of books"
    def edit_book(self):
        ""
        form = Form()
        for field in Book.FIELDS:
            form.add(StringWidget, field, title=field,
                     value=getattr(self.currentbook, field))
        form.add(SubmitWidget, "submit")
        if not form.is_submitted():
            return form.render()
        else:
            self.db.edit_book(self.currentbook.dbkey,
                              *[form[field] for field in Book.FIELDS])
            self.db.commit()
            return "The book has been edited!"
        
    def add_book(self):
        form = Form()
        for field in Book.FIELDS:
            form.add(StringWidget, field, title=field)
        form.add(SubmitWidget, "submit")
        if not form.is_submitted():
            return form.render()
        else:
            self.db.add_book(*[form[field] for field in Book.FIELDS])
            self.db.commit()
            return "A new book has been added!"
        
    def add_from_text_file(self):
        return "Please choose the book file."


root = DBInterface(BookDatabase("books"))
root.publish_show("edit_book", browser="mozilla")
