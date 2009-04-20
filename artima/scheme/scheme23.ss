#|Side effects in modules
===============================================================

In functional programs there are no side effects; in real life programs
however there are side effects, and we must be able to cope with them.
The way the module system copes with side effects is quite tricky and
implementation-dependent. The goal of this episode is to shed some
light on the subject.

Side effects at the REPL
------------------------------------------------------

Let me start with a simple example. Consider a module exporting a variable
(``x``) and a function with side effects affecting that variable (``incr-x``)):

$$experimental/mod1:

(for convenience I am storing all the code of this episode code into a
package called ``experimental``).
This kind of side effect is ruled out by the R6RS specification, since
exported variables must be immutable. This is the reason why both Ikarus
and Ypsilon reject the code with errors like 
``attempt to export mutated variable`` or 
``attempt to modify immutable variable``.
On the other hand PLT Scheme compiles the code without raising any
warning, therefore even this simple case is tricky and exposes
a non-portability of the module system :-(

Consider now a module exporting a function with side effects affecting a
non-exported variable:

$$experimental/mod2:

This is a valid library which compiles correctly. The accessor ``get-x``
gives access to the internal variable ``x``. We may import it
at the REPL and experiment with it:

.. code-block:: scheme

 > (import (experimental mod2))
 > (get-x)
 0
 > (incr-x)
 1
 > (incr-x)
 2
 > (get-x)
 2

Everything works as you would expect.

As always, things are trickier in scripts, when compiler semantics and
phase separation enters in the game.

Side effects and phase separation
------------------------------------------------------

Consider the following script:

$$experimental/use-mod2:

Here we import the module ``mod2`` twice, both at run-time and at expand time.
In Scheme implementations with full phase separation there are two fully
separated instances of the module, and running the script returns
what you would expect::

 $ plt-r6rs use-mod2.ss 
 At expand-time x=1
 At run-time x=1

The fact that ``x`` was incremented at compile-time has no effect
at all at run-time, since the run-time variable ``x`` belongs to a completely
different instance of the module. In system with weak phase separation
instead, there is only a *single instance of the module for all phases*,
so that incrementing ``x`` at expand-time has effect at runtime::

 $ ikarus --r6rs-script use-mod2.ss
 At expand-time x=1
 At run-time x=2

You would get the same with Ypsilon.

This only works because the script is executed immediately
after compilation *in the same process*. However, having compile-time
effects affecting run-time values is *utterly wrong*, since it breaks
separate compilation. If we turn the script into a library and we
compile it separately, it is clear than the run-time value of ``x``
cannot be affected by the compile-time value of ``x``
(maybe the code was compiled 10 years ago!) and it
must use a separate instance of the imported module.

Side effects and separate compilation
-------------------------------------------------------------

Let me explain in detail how separate compilation works in Ikarus,
Ypsilon and PLT Scheme. Suppose we turn the previous script into a library

$$experimental/use-mod3:

and let us invoke this library though a script ``use-mod3.ss``:

$$experimental/use-mod3:

If we use PLT Scheme, nothings changes::

 $ plt-r6rs use-mod3.ss
 At expand-time x=1
 At run-time x=1

This is expected: turning a script into a library did not make
anything magic happens. On the other hand, things are very
different if we run the same code under Ypsilon.
The first time the script is run it prints three lines::

 $ ypsilon --r6rs use-mod3.ss 
 At expand-time x=1
 At expand-time x=2
 At run-time x=3

However, if we run the script again it prints just one line::

 $ ypsilon --r6rs use-mod3.ss 
 At run-time x=1

The reason is that the first time Ypsilon compiles the libraries, using
the same module instance, so that there is a single ``x`` variable which
is incremented twice at expand time - the first time when ``mod2``
is imported and the second time when ``mod3`` is imported - and
once at run-time. The second time there is nothing
to recompile, so only the runtime ``x`` variable is incremented, and
there is no reference to the compile time instance.

The situation for Ikarus is subtler. Apparently we get the same
as before, when ``mod3`` was just a script::

 $ ikarus --r6rs-script use-mod3.ss 
 At expand-time x=1
 At run-time x=2

However, this only happens because Ikarus is compiling all the libraries
at the same time. If we use separate compilation we get::

 $ ikarus --compile-dependencies use-mod3.ss 
 At expand-time x=1
 Serializing "/home/micheles/gcode/scheme/experimental/mod3.sls.ikarus-fasl" ...
 Serializing "/home/micheles/gcode/scheme/experimental/mod2.sls.ikarus-fasl" ...

As you see, ``mod2`` the message ``At expand-time x=1`` is printed when
``mod2`` is compiled. If we run the script ``use-mod3.ss`` now, we
get just the runtime message::

 $ ikarus --r6rs-script use-mod3.ss 
 At run-time x=1

Both in Ikarus and in Ypsilon, the same invocation of the script
returns different results, depending if the libraries have been
precompiled or not. This is ugly and error prone. The full phase
separation mechanism of PLT Scheme has been designed to avoid this
problem: in PLT (and Larceny) one consistently gets always the same
result. That is good.

However, I think that both the PLT/Larceny people and the Ikarus/Ypsilon
people are wrong. The PLT/Larceny people are wrong since full phase
separation causes more problems than it solves: it is not the
right solution to this problem. On the other hand, the Ikarus/Ypsilon
people are wrong because they lack separate instantiation
of modules. However, it would be trivial to get separate instantiation
of modules for Ikarus and Ypsilon: it is enough to spawn
two separate processes, run one after the other: the
first to compile the script and its libraries, and the second to
execute it. That would make sure that incrementing
``x`` in the expansion phase could not influence the value of ``x``
at runtime: in this way they could get repeatable results.

In other words, in my own opinionated view the implementations
with partial phase separation are much closer to the right path than
implementation with full phase separation.

 --------------------------------------------------------------------------
|                          | single instantiation | multiple instantiation |
 --------------------------------------------------------------------------
| partial phase separation |      not so bad      | good                   |
| full phase separation    |    bad (Larceny)     | bad  (PLT)             |
 --------------------------------------------------------------------------


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
