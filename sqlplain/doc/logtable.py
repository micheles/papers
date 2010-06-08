from datetime import datetime
from sqlplain import do, connect, util, table

def init(db_uri):
    db = connect(db_uri)
    util.create_table(db, 'log', 'date DATETIME, message VARCHAR(255)',
                      force=True)
    return db, table.DTable('log')
