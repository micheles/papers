"""
Compare two tables
"""

from sqlplain import util

TEMPL = '''
SELECT %(csfields)s FROM %(table1)s AS a
INNER JOIN %(table2)s AS b
ON %(condition)s
WHERE %(clause)s
'''

SUB = '''
SELECT %(csfields)s FROM %(table1)s AS a
LEFT OUTER JOIN %(table2)s AS b
ON %(condition)s
WHERE %(clause)s
'''

def sub(db, table1, table2, kfields=(), dfields=()):
    """
    Returns the kfields and dfields in table1 but not in table2.
    If kfields and/or dfields are not given, they are reflected
    from the database. table1 and table2 must have the same primary key.
    """
    if not kfields:
        kfields = k1 = util.get_kfields(db, table1)
        k2 = util.get_kfields(db, table2)
        assert k1 == k2, '%s and %s have different primary key!' % (
            table1, table2)
    csfields = ', '.join('a.%s' % k for k in (kfields + dfields))
    condition = ' AND '.join('a.%s=b.%s' % (k, k) for k in kfields)
    clause = ' AND '.join('b.%s IS NULL' % k for k in kields)
    return db.execute(SUB % locals())
    
def compare(db, table1, table2, kfields=(), dfields=()):
    """
    Compare table1 and table2; returns the rows in table1 and not
    in table2, the rows in table2 and not in table1, and the rows
    in both tables such that the dfields differ.
    """
    if not kfields:
        kfields = k1 = util.get_kfields(db, table1)
        k2 = util.get_kfields(db, table2)
        assert k1 == k2, '%s and %s have different primary key!' % (
            table1, table2)

    sub12 = sub(db, table1, table2, kfields)
    sub21 = sub(db, table2, table1, kfields)
    
    csfields = ', '.join('a.%s' % k for k in kfields)
    condition = ' AND '.join('a.%s=b.%s' % (k, k) for k in kfields)
    if not dfields:
        d1 = util.get_dfields(db, table1)
        d2 = util.get_dfields(db, table2)
        assert d1 == d2, '%s and %s have different data fields!' % (
            table1, table2)
        dfields = d1
    clause = ' OR '.join('a.%s<>b.%s' % (d, d) for d in dfields)
    return sub12, sub21, db.execute(TEMPL % locals())

if __name__ == '__main__':
    from sqlplain import lazyconnect
    db = lazyconnect('dbserver2')
    #compare(db, 'riskfactor', 'riskfactor2')
    print sub(db, 'riskfactor', 'riskfactor2')
