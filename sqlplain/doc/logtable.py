from datetime import datetime
from sqlplain import do, lazyconnect, util, table

create_log_table = do("""
CREATE TABLE log(
  date DATETIME,
  message VARCHAR(255))
""")

def init(db_uri):
    db = lazyconnect(db_uri)
    util.drop_table(db, 'log', force=True)
    create_log_table(db)
    return db
