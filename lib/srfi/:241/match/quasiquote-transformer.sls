#!r6rs

;; Copyright (C) Marc Nieper-Wißkirchen (2022).  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (srfi :241 match quasiquote-transformer)
  (export quasiquote-transformer
          unquote unquote-splicing ...
          append-n-map)
  (import (rnrs (6))
          (srfi :241 match helpers))

  (define quasiquote-transformer
    (lambda (stx)
      (define who 'quasiquote)
      (define-record-type template-variable
        (nongenerative) (sealed #t) (opaque #t)
        (fields identifier expression))
      (define quasiquote-syntax-violation
        (lambda (subform msg)
          (syntax-violation 'quasiquote msg stx subform)))
      (define gen-output
        (lambda (k tmpl lvl ell?)
          (define quasiquote?
            (lambda (x)
              (and (identifier? x) (free-identifier=? x k))))
          (define gen-ellipsis
            (lambda (tmpl* out* vars* depth tmpl2)
              (let f ([depth depth] [tmpl2 tmpl2])
                (syntax-case tmpl2 ()
                  [(ell . tmpl2)
                   (ell? #'ell)
                   (f (fx+ depth 1) #'tmpl2)]
                  [tmpl2
                   (let-values ([(out2 vars2)
                                 (gen-output k #'tmpl2 0 ell?)])
		     (for-each
		      (lambda (tmpl vars)
			(when (null? vars)
			  (quasiquote-syntax-violation #'tmpl "no substitutions to repeat here")))
		      tmpl* vars*)
                     (with-syntax ([((tmp** ...) ...)
                                    (map (lambda (vars)
					   (map template-variable-identifier vars))
					 vars*)]
				   [(out1 ...) out*])
                       (values #`(append (append-n-map #,depth
						       (lambda (tmp** ...)
							 out1)
						       tmp** ...)
					 ...
                                         #,out2)
                               (append (apply append vars*) vars2))))]))))
	  (define gen-unquote*
	    (lambda (expr*)
              (with-syntax ([(tmp* ...) (generate-temporaries expr*)])
		(values #'(tmp* ...)
			(map (lambda (tmp expr)
			       (list (make-template-variable tmp expr)))
			     #'(tmp* ...) expr*)))))
	  (syntax-case tmpl (unquote unquote-splicing) ;qq is K.
            ;; (<ellipsis> <template>)
            [(ell tmpl)
             (ell? #'ell)
             (gen-output k #'tmpl lvl (lambda (x) #f))]
            ;; (quasiquote <template>)
            [`tmpl
             (quasiquote? #'quasiquote)
             (let-values ([(out vars) (gen-output k #'tmpl (fx+ lvl 1) ell?)])
               (if (null? vars)
                   (values #'`tmpl
                           '())
                   (values #`(list 'quasiquote #,out)
                           vars)))]
            ;; (unquote <template>)
            [,expr
             (fxzero? lvl)
             (with-syntax ([(tmp) (generate-temporaries '(tmp))])
               (values #'tmp (list (make-template-variable #'tmp #'expr))))]
            [,tmpl
             (let-values ([(out vars)
                           (gen-output k #'tmpl (fx- lvl 1) ell?)])
               (if (null? vars)
                   (values #'',tmpl '())
                   (values #`(list 'unquote #,out) vars)))]
            ;; ((unquote-splicing <template> ...) <ellipsis> . <template>)
	    [((unquote-splicing expr ...) ell . tmpl2)
	     (and (fxzero? lvl) (ell? #'ell))
             (let-values ([(out* vars*)
			   (gen-unquote* #'(expr ...))])
	       (gen-ellipsis #'(expr ...) out* vars* 1 #'tmpl2))]
            ;; (<template> <ellipsis> . <template>)
	    [((unquote expr ...) ell . tmpl2)
	     (and (fxzero? lvl) (ell? #'ell))
             (let-values ([(out* vars*)
			   (gen-unquote* #'(expr ...))])
	       (gen-ellipsis #'(expr ...) out* vars* 0 #'tmpl2))]
            [(tmpl1 ell . tmpl2)
             (and (fxzero? lvl) (ell? #'ell))
             (let-values ([(out1 vars1)
                           (gen-output k #'tmpl1 0 ell?)])
               (gen-ellipsis #'(tmpl1) (list out1) (list vars1) 0 #'tmpl2))]
            ;; ((unquote <template> ...) . <template>)
            [((unquote tmpl1 ...) . tmpl2)
             (let-values ([(out vars)
                           (gen-output k #'tmpl2 lvl ell?)])
               (if (fxzero? lvl)
                   (with-syntax ([(tmp ...)
                                  (generate-temporaries #'(tmpl1 ...))])
                     (values #`(cons* tmp ... #,out)
                             (append
                              (map make-template-variable #'(tmp ...) #'(tmpl1 ...))
                              vars)))
                   (let-values ([(out* vars*)
                                 (gen-output* k #'(tmpl1 ...) (fx- lvl 1) ell?)])
                     (if (and (null? vars)
                              (null? vars*))
                         (values #''((unquote-splicing tmpl1 ...) . tmpl2)
                                 '())
                         (values #`(cons (list 'unquote #,@out*) #,out)
                                 (append vars* vars))))))]

            ;; ((unquote-splicing <template> ...) . <template>)
            [((unquote-splicing tmpl1 ...) . tmpl2)
             ;; TODO: Use gen-ellipsis.
             (let-values ([(out vars)
                           (gen-output k #'tmpl2 lvl ell?)])
               (if (fxzero? lvl)
                   (with-syntax ([(tmp ...)
                                  (generate-temporaries #'(tmpl1 ...))])
                     (values #`(append tmp ... #,out)
                             (append
                              (map make-template-variable #'(tmp ...) #'(tmpl1 ...))
                              vars)))
                   (let-values ([(out* vars*)
                                 (gen-output* k #'(tmpl1 ...) (fx- lvl 1) ell?)])
                     (if (and (null? vars)
                              (null? vars*))
                         (values #''((unquote-splicing tmpl1 ...) . tmpl2)
                                 '())
                         (values #`(cons (list 'unquote-splicing #,@out*) #,out)
                                 (append vars* vars))))))]
            ;; (<element> . <element>)
            [(el1 . el2)
             (let-values ([(out1 vars1)
                           (gen-output k #'el1 lvl ell?)]
                          [(out2 vars2)
                           (gen-output k #'el2 lvl ell?)])
               (if (and (null? vars1)
                        (null? vars2))
                   (values #''(el1 . el2)
                           '())
                   (values #`(cons #,out1 #,out2)
                           (append vars1 vars2))))]
            ;; #(<element> ...)
            [#(el ...)
             (let-values ([(out vars)
                           (gen-output k #'(el ...) lvl ell?)])
               (if (null? vars)
                   (values #'#(el ...) '())
                   (values #`(list->vector #,out) vars)))]
            ;; <constant>
            [constant
             (values #''constant '())])))
      (define gen-output*
        (lambda (k tmpl* lvl ell?)
          (let f ([tmpl* tmpl*] [out* '()] [vars* '()])
            (if (null? tmpl*)
                (values (reverse out*) vars*)
                (let ([tmpl (car tmpl*)]
                      [tmpl* (cdr tmpl*)])
                  (let-values ([(out vars) (gen-output k tmpl lvl ell?)])
                    (f tmpl* (cons out out*) (append vars vars*))))))))
      (syntax-case stx ()
        [(k tmpl)
         (let-values ([(out vars)
                       (gen-output #'k #'tmpl 0 ellipsis?)])
           (with-syntax ([(x ...) (map template-variable-identifier vars)]
                         [(e ...) (map template-variable-expression vars)])
             #`(let ([x e] ...)
                 #,out)))]
        [_
         (syntax-violation who "invalid syntax" stx)])))

  (define append-n-map
    (lambda (n proc . arg*)
      (let f ([n n] [arg* arg*])
        (if (fxzero? n)
            (apply map proc arg*)
            (let ([n (fx- n 1)])
              (apply append
                     (apply map
                            (lambda arg*
                              (f n arg*))
                            arg*)))))))

  )

;; Local Variables:
;; mode: scheme
;; End:
