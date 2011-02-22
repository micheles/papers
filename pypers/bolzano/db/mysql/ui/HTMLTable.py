import os
from cycle import Cycle, chop
from quixote.form.form import Form
import quixote.form.widget as w
from quixote_utils import RootDirectory, htmltext

class HTMLTable(object):
    def __init__(self, body, header=None, maxsize=20):
        self.header = header or []
        self.body = body
        self.maxsize = maxsize
        self.cycle = Cycle(chop(self.body, maxsize))

    def make_row(self, row, typ="td"):
        return "<tr>%s</tr>" % "".join(["<%s>%s</%s>" %
                                        (typ, col, typ) for col in row])
    def gen_table(self):
        yield "<table border='1'>"
        yield self.make_row(self.header, "th")
        for row in self.cycle():
            yield self.make_row(row)
        yield "</table>"

    
    def render(self):
        return "\n".join(self.gen_table())

if __name__ == "__main__": # test

    
    class Root(RootDirectory):
        _q_exports = ["show_table"]
        table = HTMLTable([["a", i] for i in range(100)])
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

    Root().publish_show("show_table")
                

