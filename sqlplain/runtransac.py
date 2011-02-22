"""usage:
$ python %s uri "sqlcode"
or
$ python %s uri -f file.sql
"""

import sys
from sqlplain.util import openclose

def get_uri_sql(argv):
    "Extract the URI string and the SQL code from the command line"
    if len(argv) <= 2:
        raise sys.exit(__doc__ % (argv[0], argv[0]))
    uri = argv[1]
    if uri == '-f':
        raise sys.exit(__doc__ % (argv[0], argv[0]))
    for i, arg in enumerate(argv):
        if arg == '-f':
            try:
                fname = argv[i+1]
            except IndexError:
                raise RuntimeError('Missing -f argument!')
            sql = file(fname).read()
            break
    else: # if not break
        sql = argv[2]
    return uri, sql

def test():
    print get_uri_sql(['', 'uri', 'sql'])
    print get_uri_sql(['', 'uri', '-f', 'runtransac.py'])
    print get_uri_sql(['', 'uri', '-f'])
    
if __name__ == '__main__':
    uri, sql = get_uri_sql(sys.argv)
    openclose(uri, sql, autocommit=False)
