DECORATORS README
========================================================================

The ``decorators`` distribution contains the following files:

1. README.txt (your are reading it)

2. decorators.txt (the documentation in ReStructuredText format)

3. decorators.html (the documentation in HTML format)

4. decorators.pdf (the documentation in pdf format)

5. decorators.py (the heart of the distribution)

6. noconflict.py (imported by decorators, resolves metaclass conflicts)

7. doct.py (utility to extract tests from the documentation) 

8. decorators.ps (a figure included in decorators.pdf)

9. decorators.png (a figure included in decorators.html)

10. makegraph.dot (DOT script generating the figure)


``noconflict`` and ``doct`` can be used as standalone
modules too. They are documented in the on-line Python cookbook.

After running ``python doct.py decorators.txt`` a number of files will be
generated, including a module ``customdec.py`` containing the examples
of custom decorators discussed in the documentation.

If the tests fail, then there is something wrong with your Python 
installation, and I cannot help you since I don't have your machine 
at my disposal :-( It works for me both under Red Hat 7.3 and 
Windows 98SE. Notice that Python 2.3 is required.

If you use the decorators module in your code (of course, you should
not use it in production software!) and you find some bug of unexpected
behaviour, please send a bug report to me:

             MicheleSimionato@libero.it

That's all, folks. Enjoy!
