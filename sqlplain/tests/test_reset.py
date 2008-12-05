from sqlplain import lazyconnect, do

def test_postgres():
    count_customers = do('select count(*) from customer', scalar=True)
    conn = lazyconnect('postgres://python:p1tone@localhost/rcare')
    print count_customers(conn)
    conn._curs.close() # closed the cursor by hand to emulate an error
    print count_customers(conn)

def test_mssql():
    count_customers = do('select count(*) from client', scalar=True)
    conn = lazyconnect('srs_dev')
    print count_customers(conn)
    conn._curs.close() # closed the cursor by hand to emulate an error
    print count_customers(conn)

if __name__ == '__main__':
    test_mssql()
