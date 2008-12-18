"""
Take a table with a primary key and some nullable text fields.
Strip leading spaces in all text fields and
replace the fields which are all blanks with None.

usage:

$ python %s <db-uri> <table>

"""

import os, sys
from sqlplain import lazyconnect, util
from sqlplain.table import Table

def strip(value):
    "value is None or a string"
    if value is None: # do nothing
        return None
    return value.strip() or None    
            
def get_text_fields(conn, table, exclude=()):
    'Returns the text fields of a table, possibly excluding some of them'
    names = []
    for row in util.get_descr(conn, table):
        if row.type_code == 1 and row.name not in exclude:
                names.append(row.name)
    return names

def replace_blanks(conn, tablename):
    # conn.chatty = True
    pkeys = util.get_kfields(conn, tablename)
    textfields = get_text_fields(conn, tablename, exclude=pkeys)
    pkeys = ' '.join(pkeys)
    tfields = ' '.join(textfields)
    tbl = Table.type(tablename, pkeys, tfields)(conn)
    for row in tbl.allrows():
        kw = dict((tf, strip(getattr(row, tf))) for tf in textfields)
        newrow = row._replace(**kw)
        if newrow != row:
            tbl.update_row(newrow)
            print newrow
        else:
            sys.stdout.write('.')
            sys.stdout.flush()

if __name__ == '__main__':
    try:
        uri, tablename = sys.argv[1:]
    except ValueError:
        sys.exit(__doc__ % sys.argv[0])
    replace_blanks(lazyconnect(uri), tablename)
