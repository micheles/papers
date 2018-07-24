"""
wget http://www.trirand.com/jqgrid/jqGrid-3.4.1.zip

modify  jquery.jqGrid.js and change

var pathtojsfiles = "static/js/"; // need to be ajusted

"""

import os, string, threading
from sqlite3.dbapi2 import connect
from jquery_helper import Dispatcher
from jqgrid_ex import make_xml, make_jqgrid
from paste.request import parse_formvars
from paste.httpserver import serve

class MemoryModel(threading.local):

    def __init__(self):
        self.conn = connect(':memory:')
        ex = self.conn.execute
        ex('create table Letter(name varchar(32) primary key, ordinal integer)')
        for i, char in enumerate(string.letters):
            ex("insert into Letter values (?, ?)", (char, 52-i))

    getdata_query = 'select * from Letter order by %s %s limit ? offset ?'

    def getdata(self, env, resp):
        form = parse_formvars(env, include_get_vars=True)
        page = int(form['page'])
        maxrows = int(form['rows'])
        sidx = form['sidx']   
        sord = form['sord']
        query = self.getdata_query % (sidx, sord)
        data = list(self.conn.execute(query, (maxrows, (page - 1)*maxrows)))
        totalrecords = list(
            self.conn.execute('select count(*) from Letter'))[0][0]
        if not data:
            totalpages = 0
        else:
            totalpages, rest = divmod(totalrecords, maxrows)
            if rest: 
                totalpages += 1
        resp('200 OK', [('Content-type', 'text/xml')])
        return [make_xml(page, totalpages, totalrecords, data)]

def make_root_app(staticdir):
    header = ['name', 'ordinal']
    body = '''\ 
jqModal.css
<table id="list" class="scroll">
</table>
<div id="pager" class="scroll" style="text-align:center;"></div>
'''
    grid = make_jqgrid(header, ('name',), 'getdata',
                       maxrows=13, height=300)
    return Dispatcher((body, grid), {'static' : staticdir})

if __name__ == '__main__':
    model = MemoryModel()
    root = make_root_app(os.path.expanduser('~/packages/jqgrid')) 
    root.add('getdata', model.getdata)
    serve(root, '', 8000)
