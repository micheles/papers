'''
Nella prima parte di questa serie ho discusso il problema principale
dei mixin, il sovraffollamento del namespace. Il lettore
potrebbe pensare che tale problema affligga soltanto i framework di 
dimensioni medio/grandi e che non ci siano problemi ad usare i
mixin in framework piccoli. Questo è in parte vero, ma è anche
vero che spesso e volentieri i mixin sono usati a sproposito anche
in framework piccoli. In questa seconda parte illustrerò varie
alternative all'ereditarietà multipla e ai mixin *nel piccolo*,
per sistemi ad oggetti di piccole dimensioni che potreste scrivere
anche voi.

Introduzione
--------------------------------------

Come dicevo `la volta scorsa`_ la programmazione a mixin 
è una tecnica della programmazione
orientata agli oggetti che consiste nell'iniettare nel namespace di
una classe dei metodi definiti esternamente (tipicamente in un'altra
classe) direttamente o indirettamente tramite ereditarietà. 
Se il linguaggio supporta l'ereditarietà multipla (come Python)
il modo naturale di aggiungere un mixin M ad una classe C è quello
di ereditare da C e da M simultaneamente:

.. code-block:: python

 class C_with_mixin(C, M): # M is a mixin class
    pass

Alternativamente, un mixin potrebbe essere implementato aggiungendo
dei metodi alla classe, a partire da un dizionario di metodi ``M``:

.. code-block:: python

 class C_with_mixin(C):
    pass

 for name in M: # M is a dictionary of methods
    setattr(C_with_mixin, name, M[name])

Usando questa tecnica sarebbe possibile definire i mixin in Python
anche se il linguaggio non supportasse l'ereditarietà multipla.
Similmente, anche se Ruby non supporta l'ereditarietà multipla, 
supporta lo stesso
la programmazione a mixin, perché è possibile includere i metodi 
provenienti da un modulo:

.. code-block:: python

  class C_with_mixin < C:
     include M # M is a module

Un vantaggio dell'approccio di Ruby è che i moduli non hanno genitori,
mentre in Python i mixin sono tipicamente delle classi e come tali
possono avere dei genitori e per sapere cosa fa una classe di mixin
devo andare a vedermi tutti i suoi antenati. 
Rimando i rubysti a `questo articolo`_; io darò i miei esempi in Python
ma quanto dico si applica più o meno a tutti i linguaggi che supportano
i mixin (il Common Lisp però merita un discorso a parte).

.. _questo articolo: http://ruby.html.it/articoli/leggi/2597/il-mixin-in-ruby

Qual è il vantaggio dei mixin? Il vantaggio (sulla carta) 
è il riuso del codice, visto che è possibile includere tutta una serie 
di metodi con una semplice riga. Dico sulla carta perché spesso e volentieri
esistono soluzioni più pulite dei mixin per ottenere il riuso
del codice.

.. _la volta scorsa: http://stacktrace.it/articoli/2008/06/i-pericoli-della-programmazione-con-i-mixin1/

Un cattivo esempio di uso dei mixin
----------------------------------------------------------- 

Se leggete un qualunque tutorial sull'uso dei mixin
(per esempio `Using Mix-ins with Python`_ di Chuck Esterbrook che
è ben scritto e molto informativo, anche se ha un punto di vista
diametralmente opposto al mio) troverete scritto
che i mixin servono per aggiungere funzionalità alle classi con cui
si mescolano. Per esempio, potreste avere una classe mixin ``WithLog``
siffatta:

$$WithLog

Data una qualunque classe pre-esistente ``C`` senza funzionalità di
logging, potete introdurre la funzionalità di logging ereditando
da ``WithLog``:

$$C_WithLog

Un esempio d'uso è il seguente,

.. code-block:: python

 >>> c = C_WithLog()
 >>> c.log.warn("hello")

che vi stampa su stderr la scritta ``WARNING:C_WithLog:hello``.

Questo uso dei mixin è assolutamente sbagliato: perché usare
l'ereditarietà di classi se avete bisogno di un solo metodo? Tanto
vale importare quel metodo direttamente!  In generale una classe
mixin ha senso solo se avete un gruppo coeso di metodi che stanno
logicamente insieme; se avete un solo metodo, o dei metodi slegati tra
loro, ha molto più senso definire i metodi esternamente in un modulo
di utilità ed importarli nel namespace della classe direttamente:

$$CWithLog

Non ho mai visto nessuno usare questo approccio in Python, 
probabilmente perché
molta gente proveniente da altri linguaggi non sa neppure che questo è 
possibile, eppure si tratta di
una soluzione molto più pulita dell'ereditarietà. Il problema 
dell'ereditarietà è che richiede un carico cognitivo molto più
elevato: se io leggo il codice ``C_WithLog(C, WithLog)`` capisco
che ``WithLog`` è una classe, ed immediatamente mi sorgono molte
domande: quali metodi esporta ``WithLog``? c'è forse qualche metodo
di ``C`` che accidentalmente sovrascrive uno dei metodi di ``WithLog``? 
se sì, devo stare attento
ad un qualche meccanismo di cooperazione (``super``) oppure no?
quali sono gli antenati di ``WithLog``? che metodi esportano?
sono forse sovrascritti da qualcuno dei metodi di ``C``? c'è un meccanismo
di cooperazione negli antenati di ``WithLog``? D'altra parte,
se leggo ``from utility import log`` non c'è molto da capire
e molto poco di cui preoccuparsi. L'unica avvertenza in questo
caso particolare è che ci sarà un unico oggetto logger condiviso
tra tutte le istanze della classe perché
``logging.getLogger(self.__class__.__name__)`` ritornerà sempre
lo stesso oggetto. Se servono dei logger configurati
differentemente per istanze diverse è necessario sovrascrivere
l'attributo ``.log`` caso per caso, oppure usare una strategia
diversa, tipo il `dependency injection pattern`_.

In generale, definire dei metodi/proprietà all'esterno
di una classe è una tecnica molto potente che è usata troppo
poco in Python, ma che andrebbe considerata con favore per
qualunque metodo/proprietà abbastanza generico da poter essere applicato
a più di una classe. Il sistema a oggetti del Common Lisp eleva la
pratica di definire metodi all'esterno delle classi a regola, tanto
è vero che in CLOS_ si parla di funzioni generiche più che di metodi
nel senso tradizionale del termine.

In this `old post of mine`_ I actually argue than the situations where
you would like to use a mixin (i.e. you have methods that could be
reused for different classes) are actually the same situations where you would
be better served by generic functions (a.k.a. multimethods).

.. _Using Mix-ins with Python: http://www.linuxjournal.com/article/4540
.. _dependency injection pattern: http://en.wikipedia.org/wiki/Dependency_injection
.. _CLOS: http://en.wikipedia.org/wiki/CLOS
.. _old post of mine:

An acceptable usage of mixins
---------------------------------------------------------------

There are rare cases where using a mixin is acceptable. One of such examples
if the standard library class ``UserDict.DictMixin`` which is intended
specially to be used as a mixin.
In order to show its usage, let me discuss a toy application which is
however realistic.

Suppose you want to define a ``PictureContainer`` class in an application
to manage pictures and photos. A ``PictureContainer`` object may contain
inside both pictures and (nested) ``PictureContainer`` objects.
From the point of view of the Python programmer it could make sense
to implement such a class by using internally a dictionary.
A ``Picture`` object will contain information such as the picture
title, the picture date, a few methods to read and write the
picture on the storage (the file system, a relation database,
an object database like the ZODB or the AppEngine datastore_,
or anything else.

.. _datastore: http://code.google.com/appengine/docs/datastore/
.. _ZODB: http://wiki.zope.org/ZODB/guide/index.html

The first version of the ``PictureContainer`` class could be something
like that:

$$SimplePictureContainer

A questo punto, ci si rende conto che è scomodo dover chiamare
ogni volta il dizionario interno direttamente per accedere e modificare
le fotografie e che sarebbe
meglio esporre i suoi metodi all'esterno. Una soluzione implementativa
semplice ed accettabile è sfruttare ``UserDict.DictMixin``
che è fatta apposta per questo caso d'uso. Già che ci siamo,
possiamo anche aggiungere delle funzionalità di logging, di
modo che la differenza tra usare l'interfaccia di basso livello
(cioè chiamare direttamente i metodi del dizionario ``.data``)
e quella di alto livello (cioè i metodi di ``DictMixin``) è che
nel primo caso non si hanno chiamate al logger.

$$PictureContainer

Usare ``DictMixin`` in questo caso è accettabile, visto che:

1.
 ``DictMixin`` fornisce alle sue sottoclassi i metodi standard
 di un dizionario, che forniscono un gruppo coeso;
2. 
 i metodi forniti da ``DictMixin`` sono tutti già noti a chi sa 
 usare i dizionari in Python  e quindi il carico cognitivo è nullo;
3.
 ``DictMixin`` permette un riuso di codice sostanziale: noi abbiamo
 ridefinito esplicitamente soltanto 4 metodi, ma di fatto stiamo
 influenzando implicitamente altri 17 metodi: 
 ``__cmp__, __contains__, __iter__,
 __len__, __repr__, clear, get, has_key, items, iteritems,
 iterkeys, itervalues, pop, popitem, setdefault, update, values``: se
 ``DictMixin`` non ci fosse, avremmo dovuto reimplementarli tutti!

Notate bene che io dico che usare ``DictMixin`` come classe di mixin
in ereditarietà multipla è accettabile, ma non che questo sia 
la soluzione migliore.
La soluzione migliore è usare ``DictMixin`` come classe base.
Il problema di fondo è quello di un design
sbagliato; abbiamo scritto ``SimplePictureContainer`` quando non
conoscevamo l'esistenza di ``DictMixin`` ed ora a posteriori cerchiamo
di correggere l'errore tramite l'ereditarietà multipla, ma questo non
è la cosa giusta da fare. La cosa giusta da fare sarebbe modificare il
codice sorgente di ``SimplePictureContainer`` e farla derivare
direttamente da ``DictMixin``.  È chiaro poi che nel mondo reale
quasi sempre non si ha il completo controllo del codice: potremmo
avere bisogno di una libreria scritta da un terza parte con un errore
di design (o anche senza nessun errore, potrebbe essere una libreria
scritta per una versione vecchia di Python, quando ``DictMixin`` non
esisteva ancora) e non avere modo di modificare il codice sorgente.
In questo modo usare ``DictMixin`` con l'ereditarietà multipla è un
workaround assolutamente accettabile, ma sempre di workaround si
tratta, should not be traded for a clever design.

How to avoid multiple inheritance
------------------------------------------------------------------

Come ho preannunciato nell'introduzione a questa serie,
negli ultimi anni io sono diventato un forte oppositore dell'ereditarietà
multipla. Di fatto, la vedo soltanto come un utile escamotage per
risolvere problemi in
situazioni in cui non si ha il controllo del codice sorgente, 
ma non consiglio mai di partire
fin dall'inizio con un design basato sull'ereditarietà multipla;
anzi, in generale, consiglio di usare il meno possibile anche
l'ereditarietà singola!

Per esempio in questo caso, se non avessimo avuto a disposizione
l'ereditarietà multipla avremmo potuto risolvere il problema
con la composizione+delegazione:

$$PictureContainer2

Grazie al ``__getattr__`` tutti i metodi originali del 
``SimplePictureContainer`` sono a disposizione, ed inoltre sono
a disposizione tutti i metodi di ``DictMixin``, esattamente come
se avessimo usato l'ereditarietà multipla. D'altra parte, abbiamo
evitato di complicare la gerarchia. Uno svantaggio di ``PictureContainer2``
è che le sue instanze non sono  più istanze di ``SimplePictureContainer``,
quindi se nel vostro codice ci fosse stato qualche check del tipo
``isinstance(obj, SimplePictureContainer)`` (cosa sconsigliatissima
in Python <=2.5, come dicevo anche nel mio 
`terzo articolo sulla gestione dei record`_)
il check fallirebbe. La cosa è stata risolta in
Python 2.6 e superiori grazie al meccanismo delle ABC_; 
basta registrare ``SimplePictureContainer``
come ABC di ``PictureContainer2`` e il gioco è fatto.

.. _terzo articolo sulla gestione dei record: http://stacktrace.it/articoli/2008/06/gestione-dei-record-python3/

Conclusion
----------------------------------------

Il punto di vista esposto in questo articolo è che i mixin andrebbero
considerati più come un workaround (utili magari per interfacciarsi con
codice esistente o come ausilio per il debugging) che come una tecnica 
legittima da usare nel design di un'applicazione. 
Versioni recenti di Python hanno reso possibili molte valide
alternative all'ereditarietà e la tendenza generale dei framework
Python moderni è quella di favorire la `programmazione a componenti`_ al posto
dell'ereditarietà. Tenete
conto di questo quando progettate un'applicazione. Tenete anche conto
che gli svantaggi veri della programmazione a mixin si vedono soltanto
quando si ragiona in grande, per cui non ve ne accorgerete fino a che
la vostra applicazione crescerà fino ad andare fuori controllo.  Per
questo motivo nella prossima puntata discuterò come evitare i mixin in
framework di dimensioni medio/grandi. La soluzione si può riassumere 
in due massime: *usate la composizione al
posto dell'ereditarietà* e *tenete separati i namespace separati*.
Alla prossima!

.. _ABC: http://www.python.org/dev/peps/pep-3119/
.. _programmazione a componenti: http://www.muthukadan.net/docs/zca.html

'''

from UserDict import DictMixin
import pickle, logging, sys
from datetime import datetime

class SimplePictureContainer(object):

    def __init__(self, id, pictures_or_containers):
      self.id = id
      self.data = {}
      for poc in pictures_or_containers: 
        # both pictures and containers must have an .id
        self.data[poc.id] = poc


class PictureContainer(SimplePictureContainer, DictMixin):
  from utility import log

  def __getitem__(self, id):
    return self.data[id]

  def __setitem__(self, id, value):
    self.log.info('Adding or replacing %s into %s', id, self.id)
    self.data[id] = value

  def __delitem__(self, id):
    self.log.warn('Deleting %s', id)
    del self.data[id]

  def keys(self):
    return self.data.keys()

class PictureContainer2(DictMixin):
  from utility import log

  def __init__(self, id, pictures_or_containers):
    self._pc = SimplePictureContainer(id, pictures_or_containers)
    self.data = self._pc.data # avoids an indirection step

  def __getitem__(self, id):
    return self.data[id]

  def __setitem__(self, id, value):
    self.log.info('Adding or replacing %s into %s', id, self.id)
    self.data[id] = value

  def __delitem__(self, id):
    self.log.warn('Deleting %s', id)
    del self.data[id]

  def keys(self):
    return self.data.keys()

  def __getattr__(self, name):
    return getattr(self._pc, name)

# def subclass(cls, base):
#   bases = cls.__bases__
#   if bases and bases != (object,):
#       raise TypeError('Nontrivial base classes %s' % bases)
#   return type(cls.__name__, (base, object), vars(cls).copy())

# class DLPictureContainer(subclass(SimplePictureContainer, DictMixin)):
#   pass

#help(DLPictureContainer)
      
logging.basicConfig(level=logging.INFO, stream=sys.stdout)

class Picture(object):
  def __init__(self, id, location, title, date):
    self.id = id
    self.location = location
    self.title = title
    self.date = date
  def __str__(self):
    return '<%s %s>' % (self.__class__.__name__, self.id)

p1 = Picture('pic00001', '/home/micheles/mypictures/pic00001', 
             "Michele al mare", datetime(2008, 06, 10))

p2 = Picture('pic00002', '/home/micheles/mypictures/pic00002', 
             "Michele in montagna", datetime(2007, 06, 10))

vacanze = PictureContainer("vacanze", [p1, p2])

root = PictureContainer('root', [vacanze])
root['pic00001'] = p1

# for pic in root.walk(): print pic

class WithLog(object):
  @property
  def log(self):
    return logging.getLogger(self.__class__.__name__)

class C(object):
  pass

class C_WithLog(C, WithLog):
  pass

class CWithLog(C):
  from utility import log # log is a property


#     def walk(self):
#       for obj in self.data.itervalues():
#         if not isinstance(obj, self.__class__):
#           yield obj # simple object
#         else: # container object
#           for o in obj.walk():
#             yield o

if __name__ == '__main__':
  import doctest; doctest.testmod()
