from datetime import datetime
from sqlplain import do, lazyconnect, util, table

def init(db_uri):
    db = lazyconnect(db_uri)
    util.create_table(db, 'log', 'date DATETIME, message VARCHAR(255)',
                      force=True)
    return db, table.DTable('log')
