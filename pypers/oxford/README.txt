** PYTHON 2.4 IS REQUIRED TO RUN THE EXAMPLES IN THE LECTURE NOTES **

Instructions:

$ unzip oxford-lectures.zip
$ cd oxford-lectures
$ python -m doctester < all.rst

This will extracts all code snippets from the source file and run all tests.
You should get the message

doctest: run 209 tests, failed 0

If you are on Windows, you will likely miss the "crypt" module, so a few
examples may not run. Don't worry about them. I have not tried
to make the scripts portable (for instance I have hard coded the
location of optparse in the import_with_metaclass example; if the
pathname differs in your system you will have to change it).

Generally speaking these lectures are unpolished, too concise, with
more code than words, and with cutting edge/very experimental code.
Read them at your risk and peril. Take in account the fact that they
were prepared as a last minute replacement for Alex Martelli's tutorial, 
with a limited amount of time and a very advanced program to follow.

My main concern in preparing these notes was to give the readers a few
*ideas*, not polished solutions. If you are reading these notes, you
will be more than capable to customize these ideas to your own situation
and to fix the unavoidable little bugs, imperfections, annoyances.

Whereas I recommend the first lecture about iterators and generators
to everybody, take in account than the second and especially the
third lecture may cause your head to explode. I do not take any
responsability in that case.

Enjoy!

                   Michele Simionato, April 2005
