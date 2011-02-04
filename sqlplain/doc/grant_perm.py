'''
$ grant_perm $DSN schema perm role
'''

import sys
from sqlplain import connect, util

if __name__ == '__main__':
    try:
        dsn, schema, perm, role = sys.argv[1:]
    except ValueError:
        sys.exit(__doc__)
    db = connect(dsn)
    for table in util.get_tables(db, 'public'):
        db.execute('GRANT %s ON %s TO %s' % (perm, table, role))
        print 'Granted %s on %s to %s' % (perm, table, role)
