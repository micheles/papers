#|Phase separation
===============================================================

Phase separation
-------------------------------------------------

Phase separation is one of the trickiest concepts in Scheme macrology,
and perhaps the one that gave me the most headaches when I was learning
macros. It still beats me sometimes. Actually, the concept it is not
that difficult, it is its practical implementation which is extremely
tricky because it is *unspecified by the Scheme standard* and
totally *implementation-dependendent*: worse than
that, the *same* implementation can implement phase separation *differently*
in compiled code and in interpreted code, and/or differently in the REPL 
and in scripts!


If you have a macro depending on helper functions, like
the previous one, you must put the helper functions in a separated
module if you want to ensure portability. Moreover, you must
import the helper functions with the syntax ``(for (module-name) expand)``
meaning that the helper functions are intended to be used at expand
time, in macros. Ikarus is quite forgiving and can just use a regular
import, but PLT Scheme and Larceny will raise an error if you do not
use the ``for expand``. A full description of the module system, with
all the gory details, will require six more episodes, and will constitute
part V of these *Adventures*.


Consider for instance this example in Ypsilon, which tries to implement
a macro registry: 

$$registry.ypsilon:

You can run the example and you will get

``registry: ((#<syntax m>)``

as result (notice however that if you comment out the macro use, i.e.
the ``(m)`` line, the registry will *not* be populated).
So everything seems to work as one would expect.
However, if you try to run the same
example in Ikarus or in PLT Scheme or in most other R6RS Scheme
implementations you will get an error. Let me
show the PLT error message message, which is rather
clear if you understand what phase separation is, wheread
the Ikarus error message is somewhat misleading for reasons misterious to me::

 $ plt-r6rs registry.ypsilon.ss 
 registry.ypsilon.ss:10:5: compile: unbound variable in module
 (in the transformer environment, which does not include the
 run-time definition) in: register

Ypsilon, as most interpreted Scheme implementations, has no phase separation:
there is no big difference between macros and functions, which are
simply recognized in the order given by their position in the source code.
In our example the ``register`` function comes before the ``m`` macro,
so it can be used in the right hand side of the macro definition.

An example will clarify the point. Suppose we define a registry module
as follows

$$experimental/registry:

(for convenience I am storing all this
code in a package called ``experimental``) and suppose we use it as follows:

$$use-registry.ikarus:

In Ikarus everything works fine (in Ypsilon too of course)
and running the script will return you something like

::

 registering #<syntax m [char 83 of use-registry.ikarus.ss]>
 (#<syntax m [char 83 of use-registry.ikarus.ss]>)

(notice the annotation about the position of the identifier ``m`` in
the source code of the script, a signature of the fact that ``m``
is a bona fide syntax object).

In PLT Scheme instead running the script raise an error::

 $ plt-r6rs use-registry.ikarus.ss
 use-registry.ikarus.ss:5:5: compile: unbound variable in module
 (transformer environment) in: register

.. image:: salvador-dali-clock.jpg

The problem is that PLT Scheme has *strong phase separation*: by default
names defined in external modules are imported *only* at runtime.
In some sense this is absurd since
names defined in an external pre-compiled modules
are of course known at compile time
(this is why Ikarus has no trouble to import them at compile time);
nevertheless PLT Scheme and Larceny Scheme forces you to specify
at which phase the functions must be imported. If you want to import
them at expansion time (the time when macros are processed; often
incorrectly used as synonymous for compilation time) you must say so:

$$use-registry.mzscheme:

Notice also that I did specify importation at run time for the
``registry`` function, since it is called at runtime, i.e. not inside
macros. If you run this script you will get::

 $ plt-r6rs use-registry.mzscheme.ss
 registering #<syntax:/home/micheles/gcode/artima/scheme/use-registry.mzscheme.ss:8:16>
 ()

The PLT Scheme representation of syntax objects shows the line number and
the column number, therefore you should interpret the previous out as
*I am registering the syntax object defined in the source file
use-registry.mzscheme.ss, at line 8 and column 16*, which corresponds
to the identifier ``m``.

This is close, but not quite cigar. The script now runs, but it
returns a rather unexpected empty list. The reason why the registry is
empty has nothing to do with phase separation, but rather
another "feature" of PLT Scheme (and only of PLT Scheme) which goes
under the name of multiple instantiation of modules. In practice,
importing a module in PLT Scheme imports *an independent
copy (instance) of the original module*.

The imported instance of the module includes a copy of all bindings
defined in the original module,
*including the internal bindings which are not exported*.
This remark explains why the registry is empty: the ``register``
functions changes the ``_registry`` list in the *current* instance,
but the value of ``_registry`` in the *original* instance (i.e. the
value returned by the ``(registry)`` function) is left unchanged and
is the same as at the beginning, i.e. the empty list.

However, I do not want to
complicate the explanation of phase separation now, which is already
complicated as it is, so let me defer a full explanation of this point
to a future episode of my *Adventures*.
      
Discussion
-------------------------------------------------

Is phase separation a good thing?
It is clear that for the programmer's point of view, the simplest thing
is lack of phase separation. This is the semantic typically (but now
always) chosen by Scheme interpreters and REPLs: as soon as you type
it in, an helper function is available for use in macros.
If you look at it with honesty, at the end phase separation is
nothing else that a *performance hack*: by separing compilation time
from runtime you can perform some computation at compilation time only
and gain performance.

Therefore, if you have a compiled version of Scheme,
it makes sense to separate compilation time from runtime, and to
expand macros *before* compiling the helper functions (in absence of
phase separation, macros are still expanded before running any runtime
code, but *after* recognizing the helper functions).
Notice that Scheme has a concept of *macro expansion time* which is
valid even for interpreted implementation when there is no compilation
time. The `expansion process`_ of Scheme source code is specified in
the R6RS.

There is still the question if strong phase separation is a good thing,
or if weak phase separation (as in Ikarus) is enough. For the programmer
weak phase separation is easier, since he does not need to specify
the phase in which he want to import names. Strong phase separation
has been introduced so that at compile time a language which is
completely different from the language you use at runtime. In particular
you could decided to use in macros a subset of the full R6RS language.

Suppose for instance you are a teacher, and you want to force your
students to write their macros using only a functional subset of Scheme.
You could then import at compile time all R6RS procedures except the
nonfunctional ones (like ``set!``) while keeping import at runtime
the whole R6RS. You could even perform the opposite, and remove ``set!``
from the runtime, but allowing it at compile time.

Therefore strong phase separation is strictly more powerful than week
phase separation, since it gives you more control. In Ikarus, when
you import a name in your module, the name is imported in all phases,
and there is nothing you can do about it.
On the other hand strong phase separation makes everything more complicated:
it is somewhat akin to the introduction of multiple namespace, because
the same name can be imported in a given phase and not in another,
and that can lead to confusion.

There are people in the Scheme community thinking that strong phase
separation is a mistake, and that weak phase separation is the right thing
to do. On the other side people (especially from the PLT community where
all this originated) sing the virtues of strong phase separation and say
all good things about it. I personally I have not seen a compelling
use case for strong phase separation yet, and I would be happy is
some of my readers could give me such an example.
On the other hand, I am well known for preferring simplicity over
(unneeded) power. 

.. _expansion process: http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-13.html#node_chap_10

|#

(import (rnrs) (sweet-macros) (for (aps lang) expand) (aps compat))
         
;;DEF-BOOK
(def-syntax (def-book name title author)
  (: with-syntax
     name-title (identifier-append #'name "-title")
     name-author (identifier-append #'name "-author")
     #'(begin
         (define name (vector title author))
         (define name-title (vector-ref name 0))
         (define name-author (vector-ref name 1)))))

;;END
(pretty-print (syntax-expand (def-book bible "The Bible" "God")))


 ;;TEST-DEF-BOOK
 (test "def-book"
       (let ()
         (def-book bible "The Bible" "God")
         (list bible-title bible-author))
       (list "The Bible" "God"))
 ;;END

             
;;ALIST2
(def-syntax (alist2 arg ...)
  (: with-syntax ((name value) ...) (normalize #'(arg ...))
     (if (for-all identifier? #'(name ...))
         #'(let* ((name value) ...)
             (list (list 'name name) ...))
         (syntax-violation 'alist "Found non identifier" #'(name ...)
                           (remp identifier? #'(name ...))))))
;;END

(run
 (let ((a 1))
   (test "mixed"
         (alist2 a (b (* 2 a)))
         '((a 1) (b 2))))
 )
