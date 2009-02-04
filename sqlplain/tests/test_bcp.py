from sqlplain import lazyconnect
from sqlplain.mssql_util import *

srs = lazyconnect('dbserver2')

bcp_dump(srs.uri, 'client', '/tmp/client.bcpdump')

bcp_restore(srs.uri, '/tmp/client.bcpdump', 'client')
