from __future__ import with_statement
from sqlplain.sql_support import qmark2pyformat, STRING_OR_COMMENT
from ms.time_utils import Clock

TEMPL = '''
-- insert into covered
INSERT INTO rhp_covered_product 
  SELECT p.refdate, c.client_srs, c.client_id, p.pricer, ep.productcode, 
  ep.clientcode, rmtck.covered_date FROM rhp_exportedproduct AS ep
  INNER JOIN customer AS c
  ON lower(ep.client) = c.client_srs
  INNER JOIN rhp_product AS p
  ON ep.productcode = p.productcode 
  AND ep.refdate = p.refdate
  AND p.refdate = ?
  INNER JOIN (
    SELECT client_id, productcode, covered_date 
    FROM covered_by_nonrequested
    
    UNION 
    
    SELECT client_id, productcode , covered_date FROM 
    covered_by_rhp_tickerlookup 
    
    ) AS rmtck    
  ON c.client_id = rmtck.client_id
  AND rmtck.productcode = p.productcode
'''

#TEMPL = 'select * from client where client=?'

def write(dt):
    print "Spent %s ms" % dt

with Clock(write):
    for i in xrange(100000):
        qmark2pyformat(TEMPL)

def qmark2pyformat(templ): # version without cache
    qmarks = 0
    out = []
    for i, chunk in enumerate(STRING_OR_COMMENT.split(templ)):
        if i % 2 == 0: # real sql code
            qmarks += chunk.count('?')
            out.append(chunk.replace('?', '%s'))
        else: # string or comment
            out.append(chunk)
    new_templ = ''.join(out)
    return qmarks, new_templ

with Clock(write):
    for i in xrange(100000):
        qmark2pyformat(TEMPL)
