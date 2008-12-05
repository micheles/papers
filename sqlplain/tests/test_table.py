from sqlplain import lazyconnect
from sqlplain.table import table

Client = table('client', 'client', 'provider')

conn = lazyconnect('srs_dev')
client = Client(conn)

if __name__ == '__main__':
    
    print client.keys()
    print client[('adbnew',)]
    print client.select(client='adbnew')
    print client
