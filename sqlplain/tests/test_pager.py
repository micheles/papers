from sqlplain import connect
from sqlplain.pager import SqlPaginator

def setup():
    global pager
    srs = connect('dbserver2')
    pager = SqlPaginator(srs, ('select * from CSFBCreditDefaultSwaps', ()), 
                         'csfb_issuer_id')

def test1():
    page = pager.fetchpage(64, 100)
    for row in page:
        print row.csfb_issuer_id
    print 'pages, rows, size:', page.npages, page.nrows, len(page)
