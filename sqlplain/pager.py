import string

def _fetchpage_mssql(self, pageno, maxrows, query_args):
    query_templ = self.query_templ
    nrows, npages = self._rows_pages(maxrows, query_templ, query_args)
    n_end = max(nrows -  pageno * maxrows, 0)
    ofields = self.ofields
    templ = string.Template('''
    SELECT TOP ${maxrows} * FROM
    (SELECT TOP ${n_end} * FROM
    ($query_templ) AS T1
    ORDER BY ${ofields} DESC) AS T2
    ORDER BY ${ofields} ASC
    ''').substitute(locals())
    res = self.conn.execute(templ, query_args)
    res.nrows = nrows
    res.npages = npages
    return res

def _fetchpage(self, pageno, maxrows, query_args):
    query_templ = self.query_templ    
    currentrow = pageno * maxrows
    ofields = self.ofields
    templ = string.Template('''
    SELECT * FROM ($query_templ) AS tmp
    ORDER BY ${ofields} LIMIT ${maxrows} OFFSET ${currentrow}
    ''').substitute(locals())
    res = self.conn.execute(templ, query_args)
    res.nrows, res.npages = self._rows_pages(maxrows, query_templ, query_args) 
    return res

class SqlPaginator(object):        
    def __init__(self, conn, query_templ, ofields, query_args=()):
        self.conn = conn
        self.query_templ = query_templ
        self.query_args = query_args
        self.ofields = ofields
        if conn.dbtype == 'mssql':
            self._fetchpage = _fetchpage_mssql.__get__(self)
        else:
            self._fetchpage = _fetchpage.__get__(self)

    def _rows_pages(self, maxrows, query_templ, query_args):
        "Count the rows and pages returned by the query"
        ct = 'SELECT count(*) FROM (%s) AS t' % query_templ
        nrows = self.conn.execute(ct, query_args, scalar=True) 
        npages, rest = divmod(nrows, maxrows)    
        if rest:
            npages += 1
        return nrows, npages

    def fetchpage(self, pageno, maxrows, query_args=None):
        """
        Fetch a page from a paginated query. Return a list with attributes
        nrows and npages.
        """
        pageno = max(0, pageno) # makes sure pageno is positive
        if query_args is None:
            query_args = self.query_args
        return self._fetchpage(pageno, maxrows, query_args)

'''
Notice: in SQL Server 2005 there is the row_number() function which can
be used in conjunction with ORDER BY; an example is the following:
select * from
(select Row_number() over (order by csfb_issuer_id) as rn,* 
 from csfbcreditdefaultswaps) as x  
where rn between 100 and 110

'''
