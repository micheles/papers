import sys
from simplejson import dumps
from operator import itemgetter
from xml.etree.ElementTree import Element, tostring

def newElement(name, text='', elements=None, **attr):
    el = Element(name, attr)
    el.text = unicode(text)
    if elements:
        for x in elements:
            el.append(x)
    return el

# old version
#def make_xml(pageno, totalpages, totalrecords, key, data):
#    page = newElement('page', pageno)
#    total = newElement('total', totalpages)
#    records = newElement('records', totalrecords)
#    allrows = [newElement('row', id=key(cols),
#                          elements=[newElement('cell', col) for col in cols])
#               for cols in data]
#    rows = newElement('rows', elements=[page, total, records] + allrows)
#    return tostring(rows)

def make_xml(pageno, totalpages, totalrecords, data):
    page = newElement('page', pageno)
    total = newElement('total', totalpages)
    records = newElement('records', totalrecords)
    allrows = [newElement(
            'row', elements=[newElement('cell', col) for col in cols])
               for cols in data]
    rows = newElement('rows', elements=[page, total, records] + allrows)
    return tostring(rows)

JSTEMPL = '''var lastsel; 
jQuery("#list").jqGrid({
onSelectRow: function(id){ 
 if(id && id!==lastsel){ 
    jQuery('#list').restoreRow(lastsel); 
    jQuery('#list').editRow(id, true); lastsel=id; }},
url:%(url)r,
editurl:%(editurl)r,
datatype:%(datatype)r,
mtype:%(mtype)r,
colNames:%(colNames)s,
colModel:%(colModel)s,
pager:"#pager",
rowNum:%(maxrows)s,
sortname:%(sortname)r,
sortorder:%(sortorder)r,
imgpath:%(imgpath)r,
viewrecords:true,
altRows:true,
height:%(height)s,
caption:'Grid'
}).navGrid("#pager",{edit:true,add:false,del:false}); '''

# makes a GET with page, rows, sidx, sord
def make_jqgrid(fields, pkey, url, sortname=None, sortorder='asc', height=150,
                maxrows=20, rowList=(10,20,30), datatype='xml', mtype='GET',
                pager='#pager', editurl=''):
    sortname = sortname or pkey[0]
    imgpath='/static/themes/basic/images'
    colNames = fields
    colModel = dumps([dict(name=name, index=name, editable=name not in pkey) 
                      for name in fields])
    print JSTEMPL % locals()
    return JSTEMPL % locals()
