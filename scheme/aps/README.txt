:Title: The Adventures of a Pythonista in Schemeland
:Version: 0.1
:Author: Michele Simionato
:Email: michele.simionato@gmail.com
:Date: 28-Feb-2009
:Licence: BSD

The ``aps`` library is a collection of R6RS modules which have been
developed as a companion to my long running series
`The Adventures of a Pythonista in Schemeland`_.
To check that the library works, unzip the distribution 
in a directory in your Scheme path and run the tests::

 $ cd <DIRECTORY-IN-MY-SCHEME-PATH>
 $ unzip aps.zip
 $ scheme-script aps/test-all.ss

Currently all the tests pass with the latest development version of Ikarus.
They also pass with the latest development version of Ypsilon and with
PLT Scheme version 4, except for
the test "zip-with-error". However, this is an expected failure, since the
error messages are different between Ikarus, Ypsilon and PLT Scheme.
Ypsilon is easy enough to support, and I use it, so I think I will
keep supporting it in the future.

PLT Scheme is not supported as well as I would, and I will accept patches
from PLT experts willing to help me; the nontrivial part is supporting
``sweet-macros`` fully.

Larceny Scheme is not supported since it does not support the ``.IMPL.sls``
convention. When it does, it could be supported as well, expecially if I
get some help from my readers.

You should consider the libraries here in *alpha* status and subject to
change, at least until I will conclude the series.
 
.. _The Adventures of a Pythonista in Schemeland: http://www.artima.com/weblogs/viewpost.jsp?thread=238789

License
-----------

The library is distributed under a liberal BSD new-style licence::

   Redistributions of source code must retain the above copyright 
   notice, this list of conditions and the following disclaimer.
   Redistributions in bytecode form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in
   the documentation and/or other materials provided with the
   distribution. 

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
   OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
   TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
   DAMAGE.
