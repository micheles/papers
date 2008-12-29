'''\
Mixins considered harmful/1
============================================================================

A mixin is a collection of methods that can be injected into a class
statically (for instance via multiple inheritance) or dynamically,
after class creation. The advantages and disadvantages of the technique
are very mich debated. Personally, I started as a mixin entusiast but
I am currently very skeptical about their advantages and very conscious
about their shortcomings. Actually, I usually advice *against* using them.
This article and the next two ones discuss in detail the issues with
the mixins and give a few suggestion to avoid them, at least
in the context of Python programming. My point is that usually there
are better alternative than mixins, especially if you care maintainability
of your code. After all, mixins are just as bad as the ``from module import *``
form: every time you use a mixin, you are actually polluting your class
namespace, loosing track of the origin of your methods.

The overriding problem
-----------------------------------------------------------------------------

Multiple inheritance, mixins and traits are usually considered advanced 
techniques of object oriented programming, since most mainstream languages
(Java, C#, VisualBasic) do not suppport them, or the support is poor
(C++). However those techniques are pretty common in the coolest languages
out there, such as Python, which supports multiple inheritance, Ruby, 
which supports mixins, and Scala, which supports traits. Actually Scala
uses the term "traits" meaning mixins instead; there is a more specific
meaning for the term traits which is used in less known languages such as
Squeak_ and `PLT Scheme`_.

Many thinks of these tecniques as of "modern" OOP tecniques, but actually
they are quite old: for instance Flavors_, and old Lisp dialect,
already had the mixin concept built-in more that 25 years ago.
Nevertheless, those tecniques are ill known and often misused or abused,
therefore writing a paper showing the downfalls of the idea is in my
opinion worth the effort.

.. _Python: http://www.python.org
.. _Ruby: http://www.ruby-lang.org/en/
.. _Scala: http://www.scala-lang.org/
.. _ereditarietà multipla: http://en.wikipedia.org/wiki/Multiple_inheritance
.. _mixin: http://en.wikipedia.org/wiki/Mixin
.. _trait: http://www.iam.unibe.ch/~scg/Archive/Papers/Scha03aTraits.pdf

Multiple inheritance is the most general technique among the three
cited before: mixins can be seen as a restricted for of multiple
inheritance and traits as a restricted form of mixins. In other
word, it is trivial to implement mixins starting from multiple
inheritance, but the converse is not true.
Multiple inheritance is available in various languages  
(C++, Common Lisp, Python,
Eiffel, ...) but for brevity sake (and also because I am not omniscient) in
this article I will talk mostly about the Python implementation of it.
Nevertheless, many of my observations will be valid for other languages
too.

In a multiple inheritance language, a class can have more than one parent
and therefore it can inherits methods and attributes from more sources at
the same time. It is clear that maintaining code taking advantages of
multiple inheritance is not trivial, since in order to understand how
a class works, one needs to study all of its parents (and the parents
of the parents, recursively). Of course you have the same problem
even with single inheritance, when you have a deep hierarchy
(which is *bad*) but multiple inheritances adds another level
of complication. The methods of the daughter class may come
from many different sources (leading to *spaghetti inheritance*) and 
there is a strong coupling of the code: changing any method in any
ancestors has an effect on the daughter class. At some extent this
is is inevitable, since the other face of code reuse if code coupling
(you cannot have one without the other) and one has to cope with that;
however, there are issues specific to multiple inheritance and not
to other techniques of code reuse. For instance, the order of the paretns
is significant: a class *C1* inheriting from classes *M1* ed *M2* does
not necessarily behave as a class *C2* inheriting in *M2* and
*M1* where the order of the parents is inverted. 
The reason is that if there are common methods between *M1* and *M2*, i.e.
methods with the same name, the methods of *M1* have the precedence over the
methods of *M2* for the class ``C1(M1, M2)``, but the opposite is true
for the class ``C2(M2, M1)``.
Since the common methods are silently overridden and programmers are not
quite good at remembering the ordering, that may give raise to subtle bugs.

The situation is worse if one looks at the higher order ancestors:
the order of overriding of the methods (the so called MRO, Method
Resolution Order) is definitely non trivial in a multiple inheritance
hierarchy: I actually wrote a long essay on the subject, describing
the `Python MRO`_ and I address you to that reference for the details.
The point to notice is that the complication of the MRO is by design:
languages such as Python and Common Lisp where *designed* this way
to make possible method cooperation. I should also notice that this
is just one possible design: different languages may adopt different
designs. For instance the Eiffel language implements multiple
inheritance, but it raises and exception when two methods with
the same name are present: the programmer is forced to specify an
explicit renaming (this is basically what happens for traits). 
Years ago, I thought such a design to be simplistic
(even stupid) and very much inferior to Python design: nowadays
I have much more experience with real life large object oriented 
systems using multiple inheritance and I have come to appreciate
"stupid" designs. Actually, nowadays I think Smalltalk made the right choice
thirty years ago, deciding to *not* support multiple inheritance nor
mixins.

In generale non sono convinto che iniettare 
(direttamente o indirettamente, tramite l'ereditarietà) blocchi di metodi nel 
namespace di una classe sia una grande idea ed
in principio non mi piace *nessuna* di queste tecniche.  Spiegare
perché non sono convinto e spiegare che alternative utilizzare
richiederà almeno tre articoli, quindi armatevi di pazienza ;) 

.. _sanno già: http://stacktrace.it/articoli/2008/06/gestione-dei-record-python3
.. _Squeak: http://www.squeak.org/
.. _PLT Scheme: http://plt-scheme.org/
.. _cooperare: http://www.python.org/download/releases/2.2.3/descrintro/#cooperation
.. _super: http://www.python.org/doc/lib/built-in-funcs.html#l2h-72
.. _call-next-method: http://www.lisp.org/HyperSpec/Body/locfun_call-next-method.html

The problem of namespace pollution
------------------------------------------------------------------------

In practice, the overriding problem is not very frequent (it is serious
when it happens, but it *rarely* happens) since usually frameworks are
designed to mix *independent* sets of functionality. Usually one does
not need the full power of multiple inheritance: mixins or traits are
powerful enough to implement most Python frameworks. 

In general, a mixin is a method container; in Python, mixins are
usually implemented as classes, and their methods are injected
into classes via multiple inheritance, but you could as well
inject them directly. In Ruby, a language without multiple
inheritance, mixins are implemented as modules. In Ruby
the problem of method cooperation does not exist, since
there is method cooperation, i.e. nothing akin to Python
``super`` or CLOS ``call-next-method``. However, there is
still the ordering problem: mixing the module *M1* and the
module *M2* next is different than mixing the module *M2* first
and the module *M2* next if the modules contain methods with
the same name.

Traits were invented just to solve this problem: common methods
raise an error unless the programmer specifies the precedence
explicitly, or she renames the methods. After that, traits commute.
Traits are therefore the most explicit and safest technique,
whereas multiple inheritance is the most fragile technique,
with mixins in between.


trait. Si noti che un'implementazione appropriata dei trait dovrebbe includere
anche dei tool di introspezione che permettono di vedere una classe
sia come una collezione piatta di metodi sia come un'entità composta
(consiglio l'articolo originale sui trait_ che spiega questo
punto molto bene).  Purtroppo però spesso si chiamano trait quelli che
in realtà sono mixin, e i mixin sono afflitti dal problema del
sovraffollamento del namespace.  Per fare vedere cosa intendo
illustrerò due framework Python reali che fanno uso della tecnica dei
mixin: Tkinter e Zope.

Tkinter_ è un framework GUI di medie dimensioni che fa 
parte della libreria standard di Python. Ogni classe di Tkinter, 
anche la semplice ``Label``, è composta da molti mixin:

.. code-block:: python

 >>> import Tkinter, inspect
 >>> for i, cls in enumerate(inspect.getmro(Tkinter.Label)): 
 ...     # show the ancestors
 ...     print i, cls
 0 Tkinter.Label
 1 Tkinter.Widget
 2 Tkinter.BaseWidget
 3 Tkinter.Misc
 4 Tkinter.Pack
 5 Tkinter.Place
 6 Tkinter.Grid

The standard library function ``inspect.getmro(cls)``
returns a tuple with the ancestors of ``cls``, in the order specified by
the Method Resolution Order (MRO). In our example the MRO of
``Label`` contains the geometry mixins (``Grid``, ``Pack`` e ``Place``)
and the generic mixin ``Misc`` which provides a lot of functionality
by delegating to the underlying Tk library.
The classes ``BaseWidget``, ``Widget``
and ``Label`` have state and they take the role of base
classes, not mixins.

.. figure:: Label.png

Già a questo livello si capisce cosa intendo con *sovraffollamento
del namespace*.  Se usate una qualunque IDE con autocompletamento (o
anche ipython) e provate a completare l'espressione
``Tkinter.Label.``, otterrete 181 possibilità. Ora 181 attributi
distinti su una singola classe sono un pò tanti. Se provate a
scrivere

.. code-block:: python

  >>> help(Tkinter.Label)

vedrete l'origine dei vari attributi, cioè le classi da cui sono ereditati:
l'output dell'help occupa qualche centinaio di righe. 

Per fortuna Tkinter è un framework molto stabile (leggasi:
le classi base funzionano) e di solito non c'è bisogno di investigare 
le gerarchie per trovare bachi o i motivi di un comportamento
inaspettato. Inoltre Tkinter è tutto sommato un framework piccolo; anche in caso
di problemi i 181 attributi sono tanti ma non tantissimi, e con fatica uno 
potrebbe riuscire a mettersi in testa da dove derivano. Le cose però
sono molto diverse con Zope: nel caso di Zope una tipica classe
del framework è ottenuta componendo più di 20 classi mixin e contiene
più di 400 metodi. Tracciare l'origine di più di quattrocento metodi
e tenere in mente la gerarchia è praticamente impossibile. Sia 
l'autocompletamento che l'help da riga di comando diventano praticamente
inutilizzabili, ed anche la documentazione autogenerata è di fatto
illeggibile, perché troppo abbondante. Insomma,
un design basato sui mixin funziona nel piccolo, ma non scala affatto
per framework grandi, dove anzi si rivela essere un design disastroso.
L'intero framework di Zope 3 è stato disegnato con l'idea di evitare
l'abuso dei mixin di Zope 2 e di favorire la composizione al posto
dell'ereditarietà.
 
Il mio *odio* per i mixin nasce proprio dalla mia esperienza con Zope/Plone.
Comunque sospetto fortemente che quanto dirò si possa applicare a tutti 
i linguaggi che supportano i mixin (un'eccezione
è costituita dal Common Lisp, in cui i metodi vengono definiti
al di fuori della classi e quindi il problema dell'affollamento del
namespace non sussiste) per lo meno meno quando si
parla di grossi framework. Una conseguenza del sovraffollamento
del namespace è che è facile avere conflitti di nomi. Siccome
se ci sono centinaia di metodi è impossibile conoscerli tutti,
e siccome la sovrascrittura di un metodo non causa
nessun errore o warning, questo è un problema da non sottavalutare:
la *prima volta* che
ho sovrascritto una classe Plone mi è capitato esattamente questo, ho
sovrascritto un metodo predefinito senza saperlo, causando danni
difficili da diagnosticare, perché i risultati si sono visti
in una parte apparentemente indipendente del codice.


.. _Tkinter: http://www.pythonware.com/library/tkinter/introduction/x275-mixins.htm
.. _saggio sull'MRO: http://www.python.org/download/releases/2.3/mro/

How to avoid accidental overriding
------------------------------------------------------------------------

The first thing I did after being bitten by Plone was to write an
utility function to identify the overridden methods. Let me show
here a simplified version of that function, called ``warn_overriding``.
Potete usarla quando vi trovate
a lavorare con grosso framework che non conoscete a menadito.
Prima di tutto, conviene definirsi
una coppia di funzioni di utilità: una funzione
``getpublicnames`` che ritorna i nomi pubblici defini nel namespace di
un oggetto,

$$getpublicnames

e una funzione ``find_common_names`` che dato un insieme di classi ritorna
i nomi degli attributi comuni

$$find_common_names

Inoltre conviene definirsi una classe di warning opportuni:

$$OverridingWarning

A questo punto è facile implementare ``warn_overriding`` come un
decoratore di classe:

$$warn_overriding

Ed eccovi un esempio per capire come funziona il tutto. Date le classi base

$$Base
$$M1
$$M2

possiamo definire la sottoclasse

$$Child

in cui vi sono svariate sovrascritture di metodi. L'ordine delle sovrascritture
è specificato dall'MRO che in questo caso comprende
cinque classi:

.. code-block:: python

 >>> inspect.getmro(Child)
 (<class "Child">, <class "Base">, <class "M1">, <class "M2">, <type "object">)

La procedura ``find_common_names`` investiga le suddette classi 
(tranne ``object``, che non definisce nomi pubblici)
a coppie (ci sono quindi 4*3/2 = 6 combinazioni possibili)
per individuare eventuali sovrascritture e permette a
``warn_overriding`` di stampare
i warning corrispondenti:

.. code-block:: python

 >>> Child = warn_overriding(Child)
 OverridingWarning: Child.{m1, spam} overriding Base.{m1, spam}
 OverridingWarning: Child.ham overriding M1.ham
 OverridingWarning: Child.spam overriding M2.spam
 OverridingWarning: Base.spam overriding M2.spam

In Python 2.6 (correntemente in beta release) 
e superiori è possibile usare la sintassi molto più elegante

.. code-block:: python

 @warn_overriding
 class Child(Base, M1, M2):
     ...

per ottenere lo stesso risultato. I vantaggi sono molteplici:
il decoratore di classe è immediatamente incollato alla classe
che decora e quindi è molto più visibile; inoltre il warning
mostra la linea di codice corrispondente alla definizione della
classe, cioè il posto giusto in cui andare a fare i cambiamenti,
se necessari. Per evitare i warning è sufficiemente listare esplicitamente i
metodi sovrascritti, definendo l'attributo di classe ``override``.
Per, esempio provate ad aggiungere la riga::

     override = ['m1', 'spam', 'ham'] 

nella definizione di ``Child`` e vedrete che i warning spariranno.

``warn_overriding`` è piccolo tool che può aiutarvi se
vi trovate a combattere con un grosso framework che fa uso dei
mixin, ma è soltanto un palliativo, non una soluzione del problema del
sovraffollamento del namespace. La soluzione vera è non usare i mixin.
Dedicherò il resto della serie alla discussione di soluzioni alternative.
Rimanete sintonizzati per le prossime entusiasmanti puntate!
'''

def traits(*modules):
    def addtraits(cls):
        names = set(dir(cls))
        for mod in modules:
            modnames = set(n for n in vars(mod) if not n.startswith('_'))
            common_names = modnames & names
            if common_names:
                raise NameError('The names %s in module %s conflicts '
'with names in the previous traits or base class' % 
                                (list(common_names), mod.__name__))
            for name in modnames:
                setattr(cls, name, getattr(mod, name))
        return cls
    return addtraits

class CC(object):
    def a(self):
        pass

class M(object):
    def a(self):
        pass

#traits(M)(CC)

    
class A(object):
    def __init__(self, a):
        self.a = a
    def save(self): 
        print "save Às data", self.a

class B(A):
    def __init__(self, a, b):
        super(B, self).__init__(a)
        self.b = b
    def save(self): 
        print "save B's data", self.b
        super(B, self).save()
class C(A):
    def __init__(self, a, c):
        super(B, self).__init__(a)
        self.c = c
    def save(self): 
        print "save C's data", self.c 
        super(C, self).save()

class D(B, C):
    def __init__(self, a, b, c):
        super(D, self).__init__(a, b)
        self.b = b
    def save(self): 
        print "save D's data"
        super(D, self).save()

import inspect, warnings

class OverridingWarning(Warning):
    pass

def getpublicnames(obj):
    "Return the public names in obj.__dict__"
    return set(n for n in vars(obj) if not n.startswith('_'))

def find_common_names(classes):
    "Perform n*(n-1)/2 namespace overlapping checks on a set of n classes"
    n = len(classes)
    names = map(getpublicnames, classes)
    for i in range(0, n):
        for j in range(i+1, n):
            ci, cj = classes[i], classes[j]
            common = names[i] & names[j]
            if common:
                yield common, ci, cj

def warn_overriding(cls):
    """
    Print a warning for each public name which is overridden in the class
    hierarchy, unless if is listed in the "override" class attribute.
    """
    override = set(vars(cls).get("override", []))
    ancestors = inspect.getmro(cls)
    if ancestors[-1] is object: # remove the trivial ancestor <object>
        ancestors = ancestors[:-1]
    for common, c1, c2 in find_common_names(ancestors):
        overridden = ', '.join(common - override)
        if ',' in overridden: # for better display of the names
            overridden = '{%s}' % overridden
        if overridden:
            msg = '%s.%s overriding %s.%s' % (
                c1.__name__, overridden, c2.__name__, overridden)
            warnings.warn(msg, OverridingWarning, stacklevel=2)
    return cls

def check_disjoint(bases):
    for common, b1, b2 in find_common_names(bases):
        raise NameError(
            'Found common name(s) between %s and %s: %s' %
            (b1.__name__, b2.__name__, ', '.join(common)))



class CheckOverriding(type):
    def __init__(cls, name, bases, dic):
        override = set(dic.get("override", []))
        this = set(dic)
        for b in bases:
            common = ', '.join(this & getpublicnames(b) - override)
            if common:
                if ',' in common: common = '{%s}' % common
                msg = '%s.%s overriding %s.%s' % (
                    name, common, b.__name__, common)
                warnings.warn(msg, stacklevel=2)
        for common, m1, m2 in find_common_names(bases[1:]):
            if common:
                raise NameError(
                    'Found overridden name(s) between %s and %s: %s' %
                    (m1.__name__, m2.__name__, ', '.join(common)))
        super(CheckOverriding, cls).__init__(name, bases, dic)

class MetaTrait(type):
    def __new__(mcl, name, bases, dic):
        if bases[0] is object or bases[0].__name__ == 'Trait':
            cls = super(MetaTrait, mcl).__new__(mcl, name, bases, dic)
            if '_components' not in dic: # simple trait
                cls._components = [cls]
            return cls
        raise TypeError('A trait cannot be subclassed: %s' % bases[0])

    def __iter__(cls):
        for comp in cls._components:
            yield comp
            
    def __len__(cls):
        return len(list(cls))

    def __get__(cls, obj, objcls):
        return cls(obj, objcls)

    def add(cls, other):
        if hasattr(other, '__trait__'):
            raise AttributeError('Class %s has already a trait!' % other)
        other.__trait__ = cls # a descriptor
        get = getattr(other, '__getattr__', None)
        if get:
            def __getattr__(self, name):
                try:
                    return getattr(self.__trait__, name)
                except AttributeError:
                    return get(self, name)
        else:
            def __getattr__(self, name):
                return getattr(self.__trait__, name)
        other.__getattr__ = __getattr__
        return other

    def compose(cls, *others):
        if not others:
            return cls
        traits = (cls,) + others
        check_disjoint(traits)
        name = '+'.join(t.__name__ for t in traits)
        return cls.__class__(name, (Trait,), dict(_components=traits))

    def publicnames(cls):
        s = set()
        for comp in cls:
            s |= getpublicnames(cls)
        return s

    def __str__(cls):
        return "<Trait %s>" % cls.__name__

class Trait(object):
    """
    Traits are just dispatchers to their components.
    """
    __metaclass__ = MetaTrait

    def __init__(self, obj, objcls=None):
        if obj is None and objcls is None:
            raise TypeError('Please specify a class!')
        self._obj = obj
        self._objcls = objcls or obj.__class__

    def __getattribute__(self, name):
        if name.startswith('_'):
            raise NameError('Can only dispatch to public names %s' % name)
        get = object.__getattribute__
        for comp in get(self, '__class__'):
            descr = vars(comp).get(name)
            if descr is not None:
                return descr.__get__(get(self, '_obj'), get(self, '_objcls'))
        raise AttributeError(name)

def trait(trait, *others):
    "Class decorator"
    return trait.compose(*others).add

class T1(Trait):
    def m1(self):
        print 'called m1 from %s' % self

class T2(Trait):
    def m2(self):
        print 'called m2 from %s' % self

T12 = T1.compose(T2)

print T12

class C(object):
    pass

T12.add(C)


C().m1()

class Object:
    __metaclass__ = CheckOverriding

class Base(object):
    def m1(self):
        pass
    def spam(self):
        pass

class M1(object):
    def m2(self):
        pass
    def ham(self):
        pass

class M2(object):
    def m3(self):
        pass
    def spam(self):
        pass

#class C(Base, M2): pass
#warn_overriding(C)

#@warn_overriding
class Child(Base, M1, M2):
    def ham(self):
        pass
    def spam(self):
        pass
    def m1(self):
        pass
warn_overriding(Child)

'''
Differenza tra ereditarietà multipla "vera" e mixin
-----------------------------------------------------------------------------

Una domanda che molti si pongono è "qual è la differenza tra ereditarietà
multipla "vera" e mixin?". Chiaramente questa domanda ha senso in linguaggi
in cui i mixin vengono tipicamente implementati tramite ereditarietà
multipla, come Python: se state usando un linguaggio come Ruby che supporta 
i mixin ma non l'ereditarietà multipla la domanda non si pone.
La differenza è che l'ereditarietà "vera" in generale coinvolge
due o più classi che contengono stato mentre nella programmazione a
mixin c'è soltanto una classe che contiene stato (il primo genitore). 

 Spesso e volentieri i mixin sono di fatto usati come traits,
ovvero come contenitori di metodi disgiunti; il problema è che questo
non è garantito dal linguaggio e che se per caso da qualche parte
entra un metodo comune questo può venire sovrascritto senza che noi
ce ne accorgiamo.




Dove stiamo andando?
---------------------------------------------------------------------

Secondo la mia
personale opinione la tendenza corrente è quella di
restringere sempre più le libertà date dall'ereditarietà multipla,
che è troppo potente e quindi porta facilmente a programmi difficili 
da mantenere. Sequendo questa strada, lo sbocco naturale è quello
di eliminare completamente ereditarietà multipla, mixin e trait

In
questo articolo preferisco iniziare a mettere i puntini sulle i, per
spiegare esattamente cosa intendo con i termini che uso, visto che
c'è un pò di confusione nell'aria.


in generale è necessaria molta attenzione per usarli
correttamente ed nel dubbio è meglio asternersi.

 Come diceva tempo fa su i.c.l.py 
il nostro redattore Valentino Volonghi, forse Python dovrebbe prendere 
più ispirazione dal Common Lisp e meno da Java.

'''
