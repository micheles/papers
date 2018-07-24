import MySQLdb
from quixote_utils import RootDirectory, MultipageTable
from ui.HTMLTable import HTMLTable
from quixote.html import htmltext
from cycle import Cycle, chop
from quixote.form.form import Form
import quixote.form.widget as w
from quixote_utils import RootDirectory, htmltext
from stat_books_OO import BookPlotter

def getfields(cu, tablename):
    cu.execute("describe %s;" % tablename)
    return [tupl[0] for tupl in cu.fetchall()]

class Archive(RootDirectory):
    _q_exports = ["show_books", "show_table", "show_histo", "select_histo"]

    def __init__(self, db):
        self.cx = MySQLdb.connect(db=db)
        self.plotter = BookPlotter(self.cx)

    def _q_index(self):
        return "Archivio libri"

    def show_books(self):
        cu = self.cx.cursor()
        hits = cu.execute("select * from books;")
        self.table = HTMLTable(cu.fetchall(),
                               header=getfields(cu, "books"))
        return """Found %s hits.
        Click <a href='show_table'>here</a> to see the results.""" % hits

    def show_table(self):
        form = Form()
        form.add(w.SubmitWidget, "prev", "prev")
        form.add(w.SubmitWidget, "next", "next")
        if form.is_submitted():
            if form["prev"]:
                self.table.cycle.prev()
            elif form["next"]:
                self.table.cycle.next()
        return htmltext(self.table.render()) + form.render()

    def select_histo(self):
        form = Form()
        form.add(w.SingleSelectWidget, "kind", options=["genre", "nation"])
        form.add(w.SubmitWidget, "submit", "Choose!")
        if form.is_submitted():
            return self.show_histo(form["kind"])
        else:
            return form.render()
        
    def show_histo(self, kind="genre"):
        self.plotter.plot(kind, kind + ".dat")
        return """<h1>Histogram by %s</h1>
        <img src='file://%s'>
        """ % (kind, self.plotter.imagefile)
    
Archive("books").publish_show("select_histo")
