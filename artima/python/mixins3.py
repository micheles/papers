"""
The first two articles of this series were very practical in scope
and focused on concrete examples of usages of mixins in the Python
world. This third paper is a bit more theoretical, and tries to
enlighten the differences between multiple inheritance, mixins and traits,
as well as extending the analysis to languages other than Python.

How to avoid multiple inheritance
------------------------------------------------------------------

In recent years I have become a strong opponent of multiple inheritance.
It is a useful for quick and dirty hack, but I advice against starting with a
design based on multiple inheritance for new code; actually, I usually
recommend to use as little as possible even single inheritance!
In our example, we could have solved the design problem
without using multiple inheritance, just with composition + delegation:

$$PictureContainer

Thanks to the ``__getattr__`` trick, all the methods of
``SimplePictureContainer`` are available to ``PictureContainer2``,
on top of the methods coming from `DictMixin``: we did basically
fake multiple inheritance without complicating the hierarchy.
A disadvantage of ``PictureContainer2`` is that its instances are no
more instances of ``SimplePictureContainer``, therefore if your code
contained checks like ``isinstance(obj, SimplePictureContainer)``
(which is a very bad practice, at least for Python versions below Python 2.6)
the check would fail. The problem has been solved in 
Python 2.6 thanks to the Abstact Base Class mechanism (ABC_);
it is enought to register ``SimplePictureContainer`` as an ABC of
``PictureContainer2`` and you are done.

What can we do in such a situation? In my view there are at least
three possible attitudes:

1. Resignation. Acknowledge that the language allows multiple inheritance
   and mixins, which is used by many frameworks and that it will never go
   away. Therefore one should focus of discovering solution to cope with
   the situation, like the ``warn_overriding`` decorator that I introduced
   in the first article of this series; one can also write better introspection
   tools to navigate though mixins (the issue with pydoc is that it give
   *too much* information);

2. Education. Darsi da fare per rendere noti al grande pubblico i
   problemi dei mixin e convincere gli autori dei framework del futuro
   a usare design alternativi. That is what I tried to accomplish with the
   second paper in this series.

3. Research. Studiare implementazioni migliori dell'idea dei mixin:
   anche se non ci sono speranze per il linguaggio che si sta usando
   per motivi di compatibilit con il passato, la ricerca non 
   inutile perch potrebbe essere implementata nei linguaggi del futuro. 
   Lo stesso Python pu essere usato come linguaggio di 
   sperimentazione ed in questo articolo mostro come implementare
   i mixin in termini di descrittori e non di ereditarita.

An exercise in design
-------------------------------------------------------------

Since I do not like to talk in abstract, let me consider a concrete
design problem, which I will solve by using mixin classes but without
incurring in the overpopulaion issuer. To this aim, let me refer back
to my `previous article`_, specifically to the class
``PictureContainer2`` which inherits from``DictMixin``.
As I said, deriving from ``DictMixin`` is good since ``DictMixin``
provided only 19 attributes (you can see them with ``dir(DictMixin)``)
which are well known to everybody knowing Python dictionaries, therefore
the cognitive load is null.

The problem begins when you decide that you need to add features to
``PictureContainer2``.
Per esempio, se stiamo scrivendo un'applicazione GUI, potremo
aver bisogno di metodi tipo ``set_thumbnails_size, get_thumbnails_size, 
generate_thumbnails, show_thumbnails, make_picture_menu, show_picture_menu``,
eccetera; diciamo che abbiamo almeno 50 metodi che hanno a che fare
con la GUI (settaggio dei parametri, dei menu, dei bottoni, metodi
ausiliari e chi più ne ha più ne metta). Potremmo mettere tutti
questi 50 metodi in una classe mixin chiamata ``GUI`` ed ereditare
da ``DictMixin`` e da ``GUI``. 


Fin qui nulla di male. Supponiamo però che
la versione 2 della nostra applicazione debba andare su Web;
può avere allora senso implementare gli 8 metodi del protocollo HTTP
(``HEAD, GET, POST, PUT, DELETE, TRACE, OPTIONS, CONNECT``) in un'altra
classe mixin; se vogliamo dare anche la possibilità di editare le immagini
ai nostri utenti, può aver senso
anche un'interfaccia WebDAV (quindi con 7 metodi addizionali rispetto
al protocollo HTTP: ``PROPFIND, PROPPATCH, MKCOL, COPY, MOVE, LOCK, UNLOCK``).

D'altra parte, ci sono utenti che potrebbero preferire il buon vecchio
protocollo FPT per trasferire le immagini
(quindi 43 metodi ``ABOR, ALLO, APPE, CDUP, CWD, DELE, EPRT, 
EPSV, FEAT, HELP, LIST, MDTM, MLSD, MLST, MODE, MKD, NLST, NOOP, OPTS, PASS, 
PASV, PORT, PWD, QUIT, REIN, REST, RETR, RMD, RNFR, RNTO, SIZE, STAT, STOR, 
STOU, STRU, SYST, TYPE, USER, XCUP, XCWD, XMKD, XPWD, XRMD``). Infine,
ci sarà bisogno di metodi di autorizzazione vari (``is_admin, is_logged_user,
is_anonymous_user, is_picture_owner, is_friend_of``, eccetera), diciamo altri
20 metodi nella classe mixin AUTH.

A questo punto avremmo sei classi mixin (DictMixin, GUI, HTTP, WEBDAV,
FTP, AUTH) ed un totale di almeno 20 (da DictMixin) + 50 (da GUI) + 8
(da HTTP) + 7 (da WEBDAV) + 44 (da FTP) + 20 (da AUTH) = 148 metodi
derivanti dalle classi mixin. A questi vanno aggiunti i metodi
specifici della classe ``PictureContainer``.  Non è un bello scenario,
soprattutto se pensate che domani potrei avere bisogno di supportare
un'altra interfaccia e quindi di aggiungere ancora altri metodi. Nelle
mie stime sono stato conservativo, ma si fa presto a raggiunger le
centinaia di metodi.  Questo scenario non è affatto
irrealistico: è esattamente quello che è avvenuto in
Zope/Plone. 

In such a situation one must ask if there are alternative designs that
would avoid the overpopulation problem. The answer is yes, and it is the
standard one: *use composition instead of inheritance*. Everybody recommends
this practice, but yet it is not followed enough in real life.

.. _articolo precedente: mixins1.html
.. _Plone Site: http://www.phyast.pitt.edu/~micheles/python/plone-hierarchy.png

Conclusion
----------------------------------------

My position is that mixins should be considered more
of a hack than a legitimate design technique: they may be useful
when you need to integrate with pre-existing
code with a minimal offert, or as a debugging tool, when you want to
instrument a third party hierarchy, but if are designing an application
from scratch you are often better off if you do not rely on mixins.
Recent versions of Python make attractive many alternatives to inheritance
and I would say that the general trend of modern frameworks is to favor
`component programming`_ rather than inheritance. You should take in
account this fact. You should also take in account the fact that the
problems of mixin programming become visible only when programming in
the large, so you will find them only when your application will grow
out of control. In the next post I will discuss how to avoid mixins
in large frameworks. The solution is always the same: you should use
*composition instead of inheritance* and you should *keep separated
namespaces separated*.

.. _ABC: http://www.python.org/dev/peps/pep-3119/
.. _component programming: http://www.muthukadan.net/docs/zca.html
"""

class PictureContainer(DictMixin):
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
p1 = Picture('pic00001', '/home/micheles/mypictures/pic00001', 
             "Michele al mare", datetime(2008, 06, 10))

p2 = Picture('pic00002', '/home/micheles/mypictures/pic00002', 
             "Michele in montagna", datetime(2007, 06, 10))

vacanze = PictureContainer("vacanze", [p1, p2])

root = PictureContainer('root', [vacanze])
root['pic00001'] = p1

#     def walk(self):
#       for obj in self.data.itervalues():
#         if not isinstance(obj, self.__class__):
#           yield obj # simple object
#         else: # container object
#           for o in obj.walk():
#             yield o

# for pic in root.walk(): print pic
