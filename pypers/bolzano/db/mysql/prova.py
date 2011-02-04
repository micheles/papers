import MySQLdb
cx = MySQLdb.connect(db="books")
cu = cx.cursor()

def getfields(cu, tablename):
    cu.execute("describe %s;" % tablename)
    return [tupl[0] for tupl in cu.fetchall()]

print getfields(cu, "books")
