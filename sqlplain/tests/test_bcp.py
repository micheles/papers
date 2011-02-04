from sqlplain import connect
from sqlplain.mssql_util import *

srs = connect('dbserver2')

bcp_dump(srs.uri, 'client', '/tmp/client.bcpdump')

bcp_restore(srs.uri, '/tmp/client.bcpdump', 'client')
