#|
Second order macros
-------------------------------------------------------------

There is not upper limit to the level of sophistication you can reach
with macros: in particular it is possible to define higher order
macros, i.e. macros taking other macros as arguments or macros
expanding to other macros. Higher order macros allows an extremely
elegant programming style; on the other hand, they are exposed to the
risk of making the code incomprehensible and very hard to debug.
In this episode we will give a couple of examples of second order
macros taking other macros as argument.

Our first example is a generalization of the accumulator trick we
used last week to define the ``cond-`` macro. We will define a
``collecting-pairs`` macro, which as input another macro and a
sequence of arguments, and calls the input macro with the arguments
grouped in pairs.
Here is the code:

$$COLLECTING-PAIRS

``collecting-pairs`` can be used with many syntactic expressions like
``cond``, ``case``, ``syntax-rules``, et cetera. Here is an example
with the case_ expression::

 > (collecting-pairs (case 1)
       (1) 'one
       (2) 'two
       (3) 'three
       else 'unknown))
 one

Our second example if is a "colon" macro defined as follows:

$$lang:COLON

The colon macro ``:`` expects as argument another macro, the
``let-form``, which can be any binding macro such that
``(let-form ((patt value)) expr)`` is a valid syntax. For instance
``(let ((name value)) expr)`` can be rewritten as ``(: let name value
... expr)``, by removing four parenthesis. The latest version of the
``aps`` package provides a colon form in the ``(aps lang)`` module.


.. _case: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_idx_384
..  _Arc: http://www.paulgraham.com/arcll1.html

A two-level syntax
-------------------------------------------------------------

It is my convinction that a well designed programming language
should provide a two-level syntax: a simple syntax,
limited but able to cover the most common case, and a fully fledged
syntax, giving all the power which is needed, which however should be
used only rarely. The best designed programming language I know is
Python. While not perfect, Python takes full advantage of the two-level
syntax idea. For instance

Simplified syntax       Full syntax          
--------------------    --------------------

obj.attr                getattr(obj, 'attr')
x + y                   x.__add__(y)
c = C()                 c = C.__new__(C); c.__init__()


È una buona idea usare ``collecting-pairs`` ? E, più in generale, è
una buona idea escogitare trucchi per evitare le parentesi? Si tratta
di una questione sia filosofica che pratica. Probabilmente la
maggioranza dei programmatori trova più semplice scrivere codice con
meno parentesi. La filosofia di Scheme però è che è più importante
rendere semplice la generazione automatica di codice piuttosto che la
scrittura manuale di codice. Per dirla in chiaro, se state scrivendo
delle macro usando ``syntax-rules``, è molto più semplice usare la
condizionale con più parentesi ``cond`` piuttosto che ``cond-``. Il
motivo è che le parentesi vi permettono di raggruppare le espressioni
in gruppi che possono essere ripetuti o meno tramite l'operatore di
ellipsis: in pratica potete scrivere cose tipo ``(cond (cnd? do-this
...) ...)`` che non potreste scrivere con ``cond-``.  D'altra parte,
linguaggi diversi adottano filosofie differenti. Per esempio Arc_ di
Paul Graham adotta la filosofia di usare meno parentesi, ma può farlo
perché non ha un sistema di macro basato sul pattern matching (cosa
che a mio parere è un grosso *minus* rispetto a Scheme).  È possibile
salvare capra e cavoli? Ovverossia avere una sintassi con poche
parentesi quando si sta scrivendo codice ordinario e molte quando si
stanno scrivendo macro? La risposta è sì: basta duplicare i costrutti
base del linguaggio ed usare un approccio alla Python, che fornisce
sia una sintassi semplice che una di basso livello: per esempio si
potrebbe avere un ``__cond__`` con molte parentesi da usare nella
macro ed un ``cond`` con meno parentesi da usare normalmente. Questo
in teoria: nella pratica però Scheme fornisce soltanto la sintassi di
basso livello e lascia all'utente finale la libertà (o il carico che
dir si voglia) di implementarsi la sintassi di alto livello. Questo è
fatto sia per motivi politici (è un linguaggio disegnato da un
comitato, è impossibile accordarsi su una sintassi di alto livello che
piaccia a tutti) che ideologici (alla maggior parte dei programmatori
Scheme va bene così, non amano le imposizioni).

|#

(import (rnrs) (sweet-macros) (aps lang) (aps list-utils) (aps compat))

;;COLLECTING-PAIRS
(def-syntax collecting-pairs
  (syntax-match ()
    (sub (collecting-pairs (name arg ...) x1 x2 ...)
     #'(collecting-pairs "helper" (name arg ...) () x1 x2 ...))
    (sub (collecting-pairs "helper" (name arg ...) (acc ...))
     #'(name arg ... acc ...))
    (sub (collecting-pairs "helper" (name arg ...) (acc ...) x)
     #'(syntax-violation 'name "Mismatched pairs" '(name arg ... acc ... x) 'x))
    (sub (collecting-pairs "helper" (name arg ...) (acc ...) x1 x2 x3 ...)
     #'(collecting-pairs "helper" (name arg ...) (acc ... (x1 x2)) x3 ...))
    ))
;;END


(def-syntax (def-record name field ...)
  (: with-syntax
     (getter ...) (generate-temporaries #'(field ...))
     (i1 ...) (range 1 (+ (length #'(field ...)) 1))
     #`(begin
         (def-syntax name
           (syntax-match (<new> <signature> ? field ...)
              (sub (name <new>) #'record-new)
              (sub (name <signature>) #''(name field ...))
              (sub (name ?) #'record?)
              (sub (name field) #'getter)
              ...))
         (define (record-new field ...) (vector 'name field ...))
         (define (record? b) (eq? 'name (vector-ref b 0)))
         (define (getter b) (assert (record? b)) (vector-ref b i1)) ...
      )))

(def-record Book title author)
(pretty-print (syntax-expand (def-record Book title author)))

(define b ((Book <new>) "T" "A"))
(display b)
(newline)
(display (Book <signature>))
(display ((Book ?) b))


(display (syntax-expand (Book title)))
(newline)
(display ((Book title) b))
(newline)

(display ((Book author) b))

