# .. -*- coding: utf-8 -*-
"""
In the previous installment I discussed the namedtuple_ type which was
introduced in the standard library with Python 2.6 (if you are using
an older Python version you can just download the original 
`Hettinger's recipe`_. In questa puntata farò uso delle namedtuple per gestire
i record provenienti da un database a darò qualche consiglio su
come processare e come visualizzare tali record.

How to dump a database table
-------------------------------------------------------------------

The simplest approach to extract the content of a database table is
to convert it into a sequence of named tuples. We can do so by
defining a function
``get_table_from_db`` analogous to the ``get_table`` function we discussed
in the first installment of this series. I assume here a working familiarity
with the  `DB API 2`_ (aka PEP 249), the standard way to interact
with a relational database from Python:

$$easydb

Notice in particular the line

.. code-block:: python

 fields = map(itemgetter(0), cursor.description)

Here, we are extracting the field names from the ``.description`` attribute
of the cursor, which returns a list of tuples. We just take the first
element of each tuple by using ``itemgetter``, an utility function
defined in the operator_ module. You could do the same with a list
comprehension ``fields = [x[0] for x in cursor.description]`` but
``itemgetter`` is the most idiomatic solution.

The example here use the SQLite_ database, since drivers for it are
included in the standard library starting from Python 2.5; however,
you can easily adapt the code to any other database.
Finally, let me notice that if you know the database schema in
advance, you can just pass a pre-defined namedtuple to
``get_table_from_db``: there is not need to autogenerate it
from the query. This is useful if you want to give aliases to
the field names, especially in the case the name of a column
conflict with a Python keyword.

If you run the script you will get::

 $ python easydb.py
 DBTuple(id='id', descr='descr')
 DBTuple(id=1, descr=u"one")
 DBTuple(id=2, descr=u"two")


.. _DB API 2: http://www.python.org/dev/peps/pep-0249/
.. _SQLite: http://www.sqlite.org/

A higher level approach
----------------------------------------------------------

Using the DB API 2 is a very low level approach; nowadays most people
prefer to use an Object Relation Mapper (ORM); the most powerful
there is is SQLAlchemy_. Using SQLAlchemy my example can be
rewritten as

.. include-code:: sa.py

I am not a fan of ORMs. I find them too sophisticated
(*simple is better than complex*)
e nascondono l'SQL al programmatore (*explicit is better than implicit*).
Ciò detto, sono il primo a dire che ci sarebbe molto bisogno di una
DB API 3 ufficiale, di più alto livello della DB API 2, senza per questo
essere un ORM. In pratica, mi piacerebbe avere un equivalente
dell'engine di SQLAlchemy nella libreria standard, e che il *recordset*
ritornato da una query fosse costituito da namedtuple, non da tuple
ordinarie.

.. _SQLAlchemy: http://www.sqlalchemy.org/
.. _ORM: http://en.wikipedia.org/wiki/Object-relational_mapping

Generare tabelle
-----------------------------------------------------

Un lavoro comunissimo è quello di leggere dei dati da un database,
processarli e produrre come output una tabella di risultati.  L'output
potrebbe essere un file CSV di numeri da usare per un grafico
oppure semplicemente una tabella HTML da
pubblicare nel sito aziendale. Un workflow tipico è il
seguente, da leggere dall'alto verso il basso::

 <data source> 
         |
         | get_table
         |
  <initial table>
         |
         | processor
         |
  <intermediate table>
         |
         | processor
         |
 <final table>
         |
         | renderer
         |
  <output>

Il processore è un oggetto che prende
una tabella in ingresso e ritorna una tabella in uscita, eventualmente con
un numero di righe e/o di colonne diverso di quello in ingresso.
Siccome le tabelle sono degli oggetti iterabili, è naturale implementare
un processore in Python tramite un generatore che prendere
un iterabile e ritorna un iterabile.
In generale, vi possono essere più processori che agiscono uno dopo l'altro e
quindi più tabelle intermedie. L'ultimo processore ritorna la tabella
finale che viene successivamente convertita in una stringa e
salvata in un file, in formato CSV, HTML, XML o altro.

Per esempio, supponiamo di voler generare una tabella HTML.
In tal caso ci serve un (pre)processore che converte una tabella di
record astratti in una tabella di record concreti, che non sono
altro che sequenze di stringhe in cui i caratteri speciali
dell'HTML sono stati *escaped*; tale processore può essere
implementato come un semplice generatore:

$$htmlescape

Si noti che ``htmlescape`` è un processore del tutto generico che
non ha neppure bisogno che i record in ingresso siano delle namedtuple:
è sufficiente che siano delle sequenze generiche.

Il renderer finale può essere implementato come segue:

$$HtmlTable

Notate che ``HtmlTable`` può essere interpretato anche come un processore,
visto che ``HtmlTable(table)`` è un oggetto iterabile che ritorna 
blocchi di codice HTML. Il metodo ``.render`` può essere pensato
come il renderizzatore di default, ma è possibile usare dei renderizzatori
più sofisticati, in almeno due modi: 

1. tramite l'ereditarietà, ovvero derivando una sottoclasse di ``HtmlTable`` 
   e sovrascrivendo il metodo ``render``; 

2. in maniera funzionale, usando ``HtmlTable`` come un processore e passando 
   il suo output ad un renderizzatore completamente indipendente. 

Entrambe le possibilità hanno dei pro e dei contro, ma
l'approccio funzionale è più indicato se lo scopo finale
è quello di disaccoppiare il codice. Inoltre, la composizione funzionale
è concettualmente più leggera di una gerarchia di ereditarietà.
Questo assicura semplicità e maggiore scalabilità a casi più complessi.
 
È banale verificare che il tutto funziona con un semplice test:

$$test

In questo esempio ``get_test_table`` legge la tabella iniziale, ``htmlescape``
è il processore e ``HtmlTable`` è il renderer. Eseguendo il test si ottiene
la tabella seguente:

.. raw:: html

 <div class="rawhtml">
 <style>
 tr.even { background-color: lightgreen }
 tr.odd { background-color: lightgray }
 th { background-color: lightblue }
 </style>
 <table id="noname" border="1" summary="">
 <thead>
 <tr><th>A</th>
 <th>B</th>
 <th>C</th>
 <th>D</th>
 </tr>
 </thead>
 <tbody
 ><tr class="even">
 <td>1</td>
 <td>2</td>
 <td>3</td>
 <td>4</td>
 </tr>
 <tr class="odd">
 <td>5</td>
 <td>6</td>
 <td>7</td>
 <td>8</td>
 </tr>
 <tr class="even">
 <td>&gt;</td>
 <td>&lt;</td>
 <td>&amp;</td>
 <td>&quot;</td>
 </tr>
 </tbody>
 </table>
 </div>

È chiaro che l'approccio che ho delineato in questo articolo è del tutto
generale e si applica direttamente anche ad altri casi; lascio come
esercizio scrivere un processore/renderizzatore che converte
in formato XML, Latex o CSV.

I lettori delle `Avventure di un Pythonista in Schemeland`_ avranno
riconosciuto l'inflenza della programmazione funzionale.
Non si tratta di un caso fortuito: io
sono dell'idea che la conoscenza di linguaggi non-mainstream sia molto
utile anche quando si programma esclusivamente in linguaggi
mainstream.  In particolare, la conoscenza dei linguaggi funzionali vi
permette di mettere in dubbio concetti che paiono dogmi indiscutibili
in certi ambienti (tipo la "bontà" della programmazione ad oggetti) e
di aprirvi a design alternativi.  Non è un caso neppure il fatto che
Python (che fin dall'inizio non è mai stato un linguaggio a oggetti
bigotto alla Java) si stia muovendo sempre più verso soluzioni
funzionali, sia nel linguaggio core ( *list comprehensions*,
*generator expressions*, *tuple unpacking*, ecc) che nelle librerie
(*itertools*, *namedtuple*, ecc).

La miniserie non finisce qui: c'è ancora molto da dire sul
problema della visualizzazione di tabelle e a questo argomento
dedicheremo interamente la terza ed ultima parte. Ci vediamo
alla prossima, *happy hacking*!

.. _Avventure di un Pythonista in Schemeland: http://stacktrace.it/articoli/2008/02/le-avventure-di-un-pythonista-schemeland-1/
.. _operator: http://docs.python.org/library/operator.html
.. _namedtuple: http://docs.python.org/library/collections#collections.namedtuple
.. _Hettinger's recipe: http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/500261
"""

import os, cgi, easydb
from tabular_data import headtail

class HtmlTable(object):
    "Convert a sequence header+body into a HTML table"
    # this is just a pedagogic implementation, in a real implementation 
    # you should not hard-code your css at the Python level.
    name = "noname"
    border = "1"
    summary = ""
    css = """\
    <style>
    tr.even { background-color: lightgreen }
    tr.odd { background-color: lightgray }
    th { background-color: lightblue }
    </style>
    """
    def __init__(self, header_plus_body):
        self.header, self.body = headtail(header_plus_body)
            
    def render(self):
        join = os.linesep.join
        templ = '''\
        %s
        <table id="%s" border="%s" summary="%s">
        %%s
        </table>''' % (self.css, self.name, self.border, self.summary)
        head, tail = headtail(self) # post-processed head and tail
        h = '<thead>\n%s\n</thead>\n' % join(head)
        b = '<tbody>\n%s\n</tbody>\n' % join(join(r) for r in tail)
        return templ % (h+b)
  
    def __iter__(self):
        yield ['<tr>'] + ['<th>%s</th>' % h for h in self.header] + ['</tr>']
        for r, row in enumerate(self.body):
            ls = ['<tr class="%s">' % ["even", "odd"][r % 2]]
            for col in row:
                ls.append('<td>%s</td>' % col)
            ls.append('</tr>')
            yield ls

def htmlescape(table):
    "Converts a table of records into a table of HTML-escaped records"
    for rec in table:
        yield [cgi.escape(str(field), quote=True) for field in rec]

def test():
    page = """\
    <html>
    <head>
    </head>
    <body>
     %s
    </body>
    </html>
    """
    def get_test_table():
        return 'ABCD', '1234', '5678', '><&"'
    t = HtmlTable(htmlescape(get_test_table()))
    print >> file('output.html', 'w'), page % t.render()

if __name__ == '__main__':
    test()
