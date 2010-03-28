import MySQLdb as m

conn = m.connect()
c = conn.cursor()
print dir(c)
print c.execute("use books;")
print c.execute("select * from books;")
print c.fetchone()
print c.fetchmany(3)
print c.fetchall()
