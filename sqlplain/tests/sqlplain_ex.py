from sqlplain import DB

def test(autocommit):
    db = DB('mssql://python:p1tone@192.168.0.129/srsdevbak', autocommit)
    print db
    print db.execute('select * from client')
    print db.execute("update client set client='pippo' where 1=2")
    

    db = DB('postgres://python:p1tone@localhost/rcare', autocommit)
    print db
    print db.execute('select * from customer')
    print db.execute("update customer set client_id='pippo' where 1=2")

    db = DB('sqlite:///:memory:', autocommit)
    print db
    print db._raw_execute('create table prova (i integer)', (), None)
    print db.execute("insert into prova values (1)")
    print db.execute("select * from prova")

if __name__ == '__main__':
    test(autocommit=False)
    test(autocommit=True)

