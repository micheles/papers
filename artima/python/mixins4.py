Una possibile soluzione
---------------------------------------------------------------

La mia soluzione, come anticipato nell'`articolo precedente`_ sarà
quella di sostituire l'ereditarietà con la composizione + delegazione,
ovverossia fare uso di oggetti proxy.
Il carico cognitivo richiesto da un proxy - un
oggetto che fa dispatch su di un altro oggetto - è molto
inferiore al carico cognitivo imposto dall'ereditarietà.

Se un oggetto è un' instanza di una classe, mi sento obbligato
a conoscere tutti i suoi metodi (inclusi quelli di tutti i suoi
antenati) se non altro perché potrei sovrascriverli accidentalmente
mentre se un oggetto è un proxy mi basta sapere qual è
l'oggetto a cui si riferisce,
so che se voglio posso andare a vedere i metodi dell'oggetto, 
ma non mi sento obbligato a farlo.
È più un motivo psicologico che altro, ma il proxy piace perché
permette di tenere confinata la complessità, mentre l'ereditarietà
la espone direttamente.

Quest'ultimo punto è estremamente importante. Il cervello
umano può memorizzare un numero limitato di cose. Un
oggetto con dieci metodi può essere memorizzato abbastanza agevolmente
mentre un oggetto con cento metodi esce dalle capacità del programmatore
medio. La soluzione è quella di dividire i cento metodi in dieci categorie
con dieci metodi ognuna: a questo punto le dieci categorie possono essere
tenute in mente. La soluzione gerarchica scala: se avessi bisogno di
mille metodi, basta definire dieci macro-categorie, ognuna contenente
dieci categorie semplici, e tenere a mente le macro-categorie. La
catalogazione gerarchica è la maniera naturale per la mente umana
per memorizzare l'informazione, come formalizzato per lo meno `dai tempi 
di Aristotele`_, quindi è questa la cosa giusta
da fare, non tenere i mille metodi tutti sullo stesso piano nello stesso
namespace.

Nel caso in esame, ho deciso di trasformare tutte le classi di mixin in
proxy: se un attributo non viene trovato nel namespace del mixin, viene
cercato anche nel namespace dell'oggetto sottostante.
Tecnicamente questa idea può essere implementata definendo
degli oggetti *dispatcher* 
da usare come attributi di classe (*attribute descriptors*).
Potete trovare l'implementazione in appendice.
Un esempio d'uso è il seguente:

$$PictureContainer

Notate come io abbia rifattorizzato la classe ``PictureContainer2``
dell'`articolo precedente`_ per renderla più pulita.
In particolare ho definito esplicitamente la proprietà ``log`` invece
di importarla dal modulo di utilità (``from utility import log``)
come fatto. Tutto sommato, visto che si
tratta di tre sole righe, può avere senso riscriverle ed evitare
al lettore di andare a guardare un altro modulo. C'è sempre da trovare
un punto di equilibrio tra riuso del codice da una parte e codice
spaghetti dall'altra. Nel dubbio, tenete conto che *readability counts*.

Come vedete è sparita l'ereditarietà da tutte le classi mixin
tranne ``DictMixin``. 
Un oggetto ``PictureContainer`` *è* un dizionario e quindi
è giusto che erediti da ``DictMixin``; ha meno senso che erediti
da GUI, HTTP, WEBDAV, FTP, AUTH, perché è uno stiracchiare la
realtà dire che ``PictureContainer`` sia anche un oggetto GUI, HTTP, WEBDAV, 
FTP, AUTH.

Ho ereditato esplicitamente da ``object``, perché la classe
``DictMixin`` è una cosiddetta classe *old-style* (per motivi di
compatibilità con il passato), mentre il ``dispatcher``
è pensato per essere usato con classi *new-style*; ereditare da ``object``
fa sì che ``PictureContainer`` diventi una classe new-style. Questo
è uno dei casi in cui l'ereditarietà multipla è comoda, ma il caso
d'uso sparirà in Python 3.0, in cui tutte le classi sono new-style.

.. _Zen di Python: http://www.python.org/dev/peps/pep-0020/

Discussione
-----------------------------------------------------------------

Proviamo adesso che la nostra soluzione al problema di design
è consistente con i principi enunciati
nello `Zen di Python`_. Il fatto che l'implementazione del dispatcher
sia semplice - circa 20 righe di codice - 
è già un primo passo nella direzione giusta
(*if the implementation is hard to
explain, it's a bad idea*). Il fatto che tutti i metodi dei mixin restino 
localizzati nel loro proprio namespace senza inquinare il namespace 
della classe figlia è ancora più pythonico
(*namespaces are one honking great idea -- let's do more of those!*).
Infine, il fatto che per accedere al metodo ``POST`` del mixin ``HTTP`` 
dobbiamo scrivere esplicitamente ``self.http.POST`` è pythonicissimo
perché *explicit is better than implicit*; in questo modo
non c'è bisogno di tirare ad indovinare per capire la provenienza
di un metodo (*in the face of ambiguity, refuse the temptation to guess*).

La nostra soluzione, comunque, non è soltanto pythonica, ma anche
usabile (*practicality beats purity*): provate ad istanziare un
oggetto ``PictureContainer`` e a fare
qualche esperimento dalla console interattiva:

.. code-block:: python

 >>> pc = PictureContainer('root', [])

Vedrete che l'autocompletamento funziona perfettamente 

.. code-block:: python

 >>> pc.ftp. # press TAB
 pc.ftp.RECV                   
 pc.ftp.SEND       
 ...

che l'help
non vi sommerge di informazioni inutili

.. code-block:: python

 >>> help(pc)
 Help on PictureContainer in module mixins2 object:
 class PictureContainer(UserDict.DictMixin, __builtin__.object)
 |  Method resolution order:
 |      PictureContainer
 |      UserDict.DictMixin
 |      __builtin__.object
 |  
 |  Methods defined here:
 |  ...
 | 
 |  auth = <AUTHDispatcher {is_admin ...} >
 |  ftp = <FTPDispatcher {RECV, SEND ...} >
 |  gui = <GUIDispatcher {draw_button, draw_buttons ...} >
 |  http = <HTTPDispatcher {GET, POST ...} >
 |  webdav = <WEBDAVDispatcher {GET, POST, LOCK, UNLOCK ...} >
 ...

e potete fare introspezione soltanto sulle caratteristiche che
vi interessano, senza che tutto sia mescolato (*sparse is better than dense*):

 >>> print dir(pc.http)
 ['GET, 'POST', ...]

È anche molto facile adattare un oggetto composto da dispatchers,
per renderlo compatibile con diverse interfacce. Ma per spiegare 
propriamente questo punto mi servirebbe
un'altra serie di articoli, che potrei intitolare *I vantaggi della
programmazione a componenti*. Per il momento, mi fermo qui,
sperando di avervi dato qualche spunto interessante. Lascio ai
più curiosi l'implementazione del ``dispatcher``::

 $ cat mdispatcher.py

$$mdispatcher

.. _dai tempi di Aristotele: http://it.wikipedia.org/w/index.php?title=Categoria_aristotelica
'''

import os, copy, mdispatcher
from mdispatcher import dispatcher
from UserDict import DictMixin

class GUI(object):
  def draw_button(self, button):
    pass

  def draw_buttons(self, buttons):
    for b in buttons:
      self.draw_button(b)

class HTTP(object):
  def GET(self):
    pass
  def POST(self):
    pass

class FTP(object):
  def RECV(self):
    pass
  def SEND(self):
    pass

class WEBDAV(HTTP):
  def LOCK(self):
    passv
  def UNLOCK(self):
    pass

class AUTH(object):
  def is_admin(self):
    pass

class WebPictureContainer(DictMixin):
    http = dispatcher(HTTP)
    ftp = dispatcher(FTP)
    webdav = dispatcher(WEBDAV)

    def __init__(self, id, pictures_or_containers):
      self.id = id
      for poc in pictures_or_containers:
        self[poc.id] = poc
      
import pickle, logging, sys
from datetime import datetime
logging.basicConfig(level=logging.INFO)

def test():
    c = Container()
    c.http, c.ftp, c.webdav
    print dir(c.http)
    print dir(c.webdav)
    pickle.loads(pickle.dumps(c)).http._obj

class Picture(object):
  def __init__(self, id, location, title, date):
    self.id = id
    self.location = location
    self.title = title
    self.date = date
  def __str__(self):
    return '<%s %s>' % (self.__class__.__name__, self.id)

def walk(container, path='/'):
  for name, obj in container.items():
    newpath = os.path.join(path, name)
    if type(obj) is type(container): # container object
      for n, o in walk(obj, newpath):
        yield n, o
    else: # simple object
      yield newpath, obj

class PictureContainer(DictMixin, object):
  # dispatchers objects have the same methods as the corresponding mixin
  # class; moreover they dispatch on self, i.e. on PictureContainer objects
  gui = dispatcher(GUI)
  http = dispatcher(HTTP)
  webdav = dispatcher(WEBDAV)
  ftp = dispatcher(FTP)
  auth = dispatcher(AUTH)

  @property
  def log(self):
    return logging.getLogger(self.__class__.__name__)

  def __init__(self, id, pictures_or_containers):
      self.id = id
      self.data = {}
      for poc in pictures_or_containers: 
        # both pictures and containers must have an .id
        self.data[poc.id] = poc

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

def makecontainer():
  p1 = Picture('pic00001', '/home/micheles/mypictures/pic00001', 
              "Michele al mare", datetime(2008, 06, 10))

  p2 = Picture('pic00002', '/home/micheles/mypictures/pic00002', 
              "Michele in montagna", datetime(2007, 06, 10))

  vacanze = PictureContainer("vacanze", [p1, p2])

  root = PictureContainer('root', [vacanze])
  root['pic00001'] = p1
  return root

pc = PictureContainer('root', [])

gui = pc.gui

#help(pc)
#print dir(root.http)


if __name__ == '__main__': # test
    class M(object):
        def helper(self):
            return 1
        def add1(self, x):
            return self.helper() + x
    class C(object):
        m = dispatcher(M)
    c = C()
    print c.m.add1(2)

'''
Parafrasando il celebre articolo di Dijkstra_ del 1969 
`Goto Considered Harmful`_, potremmo titolare questa parte 
*Mixins Considered Harmful*. Notate che una ricerca con Google 
mi dà 14700 hits per "Mixins Considered Harmful", quindi non sono
l'unico a pensarla così ma comunque secondo me c'è ancora bisogno 
di parlare male dei mixins.

.. _Dijkstra: http://en.wikipedia.org/wiki/Edsger_Dijkstra
.. _Goto Considered Harmful: http://www.cs.utexas.edu/users/EWD/ewd02xx/EWD215.PDF

'''
