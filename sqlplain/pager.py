import string

template_mssql = string.Template('''
    SELECT TOP ${maxrows} * FROM
    (SELECT TOP ${n_end} * FROM
    ($query_templ) AS T1
    ORDER BY ${ofields} DESC) AS T2
    ORDER BY ${ofields} ASC
    ''')

template = string.Template('''
    SELECT * FROM ($query_templ) AS tmp
    ORDER BY ${ofields} LIMIT ${maxrows} OFFSET ${currentrow}
    ''')

def _fetchpage_mssql(self, pageno, maxrows, query_pair=None):
    query_templ, query_args = query_pair or self.query_pair
    n_end = max(self._nrows(query_templ, query_args) -  pageno * maxrows, 0)
    ofields = self.ofields
    return self.conn.execute(self.template.substitute(locals()), query_args)

def _fetchpage(self, pageno, maxrows, query_pair=None):
    query_templ, query_args = query_pair or self.query_pair
    self._nrows(query_templ, query_args) # set ._len
    currentrow = pageno * maxrows
    ofields = self.ofields
    return self.conn.execute(self.template.substitute(locals()), query_args)

class SqlPaginator(object):        
    def __init__(self, conn, query_pair, ofields):
        self.conn = conn
        self.query_pair = query_pair
        self.ofields = ofields
        if conn.dbtype == 'mssql':
            self.template = template_mssql
            self._fetchpage = _fetchpage_mssql.__get__(self)
        else:
            self.template = template
            self._fetchpage = _fetchpage.__get__(self)

    def _nrows(self, query_templ, query_args):
        qt = 'SELECT count(*) FROM (%s) AS t' % query_templ
        self._len = self.conn(qt, query_args, scalar=True)
        return self._len
    
    def _npages(self, maxrows, query_pair=None):
        npages, rest = divmod(self.nrows(query_pair), maxrows)    
        if rest: npages += 1
        return npages

    def fetchpage(self, pageno, maxrows, query_obj=None):
        """
        Fetch a page from a paginated query and return a metadata dictionary
        containing the number of rows fetched, the total number of pages,
        the row number of the first row and of the last row.
        """
        rows = self._fetchpage(
            pageno, maxrows, query_obj or self.query_obj)
        metadata = dict(
            nrows = self._len,
            npages = self._npages(maxrows, query_obj),
            beg = pageno * maxrows + 1,
            end = min(self.nrows, (pageno + 1) * maxrows),
            )
        return rows, metadata


'''
Notice: in SQL Server 2005 there is the row_number() function which can
be used in conjunction with ORDER BY; an example is the following:
select * from
(select Row_number() over (order by csfb_issuer_id) as rn,* 
 from csfbcreditdefaultswaps) as x  
where rn between 100 and 110

'''
