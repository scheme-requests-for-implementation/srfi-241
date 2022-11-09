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

(library (srfi :241 match helpers)
  (export ellipsis? length+ split-at)
  (import (rnrs))

  (define ellipsis?
    (lambda (x)
      (and (identifier? x)
	   (free-identifier=? x #'(... ...)))))

    (define length+
    (lambda (x)
      (let f ([x x] [y x] [n 0])
        (if (pair? x)
            (let ([x (cdr x)]
                  [n (fx+ n 1)])
              (if (pair? x)
                  (let ([x (cdr x)]
                        [y (cdr y)]
                        [n (fx+ n 1)])
                    (and (not (eq? x y))
                         (f x y n)))
                  n))
            n))))

  (define split-at
    (lambda (ls k)
      (let f ([ls ls] [k k])
        (if (fxzero? k)
            (values '() ls)
            (let-values ([(ls1 ls2)
                          (f (cdr ls) (fx- k 1))])
              (values (cons (car ls) ls1) ls2)))))))

;; Local Variables:
;; mode: scheme
;; End:
