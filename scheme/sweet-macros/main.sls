(library (sweet-macros)
(export syntax-match def-syntax syntax-expand local)
(import (rnrs))

;;LOCAL
(define-syntax local
  (lambda (x)
    (syntax-case x (syntax-match)
      ((local expr)
       #'expr)
      ((local (let-form name value) ... (syntax-match rest ...))
       #'(syntax-match (local (let-form name value) ...) rest ...))
      ((local (let-form name value) (l n v) ... expr)
       #'(let-form ((name value)) (local (l n v) ... expr))))
    ))
;;END

;;GUARDED-SYNTAX-CASE
(define-syntax guarded-syntax-case
  (let ((add-clause
         (lambda (clause acc)
           (syntax-case clause ()
             ((pattern skeleton . rest)
                (syntax-case #'rest ()
                  ((cond? else1 else2 ...)
                   (cons*
                    #'(pattern cond? skeleton)
                    #'(pattern (begin else1 else2 ...))
                    acc))
                  ((cond?)
                   (cons #'(pattern cond? skeleton) acc))
                  (()
                   (cons #'(pattern skeleton) acc))
                  ))))))
    (lambda (x)
      (syntax-case x ()
        ((guarded-syntax-case () (literal ...) clause ...)
         #'(lambda (y) (guarded-syntax-case y (literal ...) clause ...)))
        ((guarded-syntax-case y (literal ...) clause ...)
         (with-syntax
             (((c ...) (fold-right add-clause '() #'(clause ...))))
           #'(syntax-case y (literal ...) c ...)))
        ))))
;;END

;;SYNTAX-MATCH
(define-syntax syntax-match
  (guarded-syntax-case () (sub local)
    ((self (local (let-form name value) ...) (literal ...)
           (sub patt skel . rest) ...)
     #'(local (let-form name value) ...
         (guarded-syntax-case ()
           (<literals> <patterns> <source> <transformer> literal ...)
           ((ctx <literals>)
            #''(literal ...))
           ((ctx <patterns>)
            #''((... (... patt)) ...))
           ((ctx <source>)
            #''(self (local (let-form name value) ...) (literal ...)
                     (... (... (sub patt skel . rest))) ...))
           ((ctx <transformer>)
            #'(self (local (let-form name value) ...) (literal ...)
                    (... (... (sub patt skel . rest))) ...))
           (patt skel . rest) ...))
     (for-all identifier? #'(literal ...))
     (syntax-violation 'syntax-match "Found non identifier" #'(literal ...)
                       (remp identifier? #'(literal ...))))
    
    ((self (literal ...) (sub patt skel . rest) ...)
     #'(self (local)(literal ...) (sub patt skel . rest) ...))

    ((self x (literal ...) (sub patt skel . rest) ...)
     #'(guarded-syntax-case x (literal ...) (patt skel . rest) ...))
    ))
;;END

;; DEF-SYNTAX
(define-syntax def-syntax
  (syntax-match (extends local)
    (sub (def-syntax name (extends parent)
       (local loc ...) (literal ...) 
       clause ...)
     #'(define-syntax name
         (syntax-match (local loc ...) (literal ...)
           clause ...
           (sub x ((parent <transformer>) #'x)))))
    (sub (def-syntax (name . args) skel . rest)
     #'(define-syntax name (syntax-match () (sub (name . args) skel . rest))))
    (sub (def-syntax name transformer)
     #'(define-syntax name transformer))
    ))
;;END

;;SYNTAX-EXPAND
(def-syntax (syntax-expand (macro . args))
  #'(syntax->datum ((macro <transformer>) #'(... (... (macro . args))))))
;;END

)
;;;                             LEGALESE 

;;   Redistributions of source code must retain the above copyright 
;;   notice, this list of conditions and the following disclaimer.
;;   Redistributions in bytecode form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in
;;   the documentation and/or other materials provided with the
;;   distribution. 

;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;   HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;;   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
;;   OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;;   ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
;;   TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;   USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;   DAMAGE.
