from sqlplain import lazyconnect, do
from cStringIO import StringIO
#from sqlplain.postgres_util import get_schema_postgres
#print get_schema_postgres(rcare.uri, 'customer')

rcare = lazyconnect('rcare')
rcare_prod = lazyconnect('rcare_prod')

CREATE_CUSTOMER = '''
CREATE TABLE $customer (
    client_id character varying(32) PRIMARY KEY,
    description character varying,
    email character varying,
    short_description character varying(16),
    mail_report_info character varying(128),
    attach_prefix character varying(32),
    filter_from character varying(32),
    zope_id character varying(32),
    lookup boolean,
    client_srs character varying(32),
    CONSTRAINT nnull_id_short_desc CHECK ((short_description IS NOT NULL))
);
'''

def copy_from(src, dest, table):
    out = StringIO()
    src._curs.copy_to(out, 'customer')
    drop_table(dest, 'customer_prod', force=True):
    dest.executescript(CREATE_CUSTOMER, customer='customer_prod')
    dest._curs.copy_from(out, 'customer_prod')
    out.close()
