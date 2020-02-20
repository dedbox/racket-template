;; Copyright 2020 Eric Griffis <dedbox@gmail.com>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

#lang racket/base

(require template (for-syntax racket/base template))

(module+ test
  (require racket/function
           rackunit
           rackunit/text-ui
           (for-syntax racket/base
                       racket/sequence))

  (provide the-tests)

  (define (run-all-tests)
    (run-tests the-tests)
    (void))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-test-suite the-tests primitives combiners)

  (define-test-suite primitives
    (test-case "static"
      (check  free-identifier=? (with-template () #'X) #'X)
      (check bound-identifier=? (with-template () #'X) #'X)
      (check free-identifier=?
             (with-template () #'X)
             (with-template () #'X))
      (check bound-identifier=?
             (with-template () #'X)
             (with-template () #'X)))

    (test-case "generated"
      (check  free-identifier=? (with-template ([$x A]) #'$x$x) #'AA)
      (check bound-identifier=? (with-template ([$x A]) #'$x$x) #'AA)
      (check free-identifier=?
             (with-template ([$x A]) #'$x$x)
             (with-template ([$x A]) #'$x$x))
      (check bound-identifier=?
             (with-template ([$x A]) #'$x$x)
             (with-template ([$x A]) #'$x$x)))

    (test-case "quote-generated"
      (check free-identifier=?  (quote-template ([$x A]) #'$x$x) #'AA)
      (check bound-identifier=? (quote-template ([$x A]) #'$x$x) #'AA)
      (check free-identifier=?
             (quote-template ([$x A]) #'$x$x)
             (quote-template ([$x A]) #'$x$x))
      (check bound-identifier=?
             (quote-template ([$x A]) #'$x$x)
             (quote-template ([$x A]) #'$x$x)))

    (test-case "untemplate-generated"
      (check  free-identifier=? (with-template () #'(untemplate 'B)) #'B)
      (check bound-identifier=? (with-template () #'(untemplate 'B)) #'B)
      (check equal? (quote-template () '#`#,(untemplate ''B)) '#`#,(untemplate ''B))
      (check free-identifier=?
             (with-template () #'(untemplate 'B))
             (with-template () #'(untemplate 'B)))
      (check bound-identifier=?
             (with-template () #'(untemplate 'B))
             (with-template () #'(untemplate 'B)))
      (check equal?
             (quote-template () '#`#,(untemplate ''B))
             (quote-template () '#`#,(untemplate ''B)))
      (check free-identifier=? (with-template () #'(untemplate #'C)) #'C)
      (check (negate bound-identifier=?) (with-template () #'(untemplate #'C)) #'C)
      (check free-identifier=?
             (with-template () #'(untemplate #'C))
             (with-template () #'(untemplate #'C)))
      (check (negate bound-identifier=?)
             (with-template () #'(untemplate #'C))
             (with-template () #'(untemplate #'C)))
      (check equal? (quote-template () '#`#,(untemplate #''C)) '#`#,(untemplate #''C))
      (check equal?
             (quote-template () '#`#,(untemplate #''C))
             (quote-template () '#`#,(untemplate #''C)))))

  (define-test-suite combiners
    (test-case "begin-template"
      (check  free-identifier=? (begin-template #'A) #'A)
      (check bound-identifier=? (begin-template #'A) #'A))

    (test-case "begin0-template"
      (check  free-identifier=? (begin0-template #'A) #'A)
      (check bound-identifier=? (begin0-template #'A) #'A))

    (test-case "if-template"
      (check  free-identifier=? (if-template #t #'A #'B) #'A)
      (check  free-identifier=? (if-template #f #'A #'B) #'B)
      (check bound-identifier=? (if-template #t #'A #'B) #'A)
      (check bound-identifier=? (if-template #f #'A #'B) #'B))

    (test-case "cond-template"
      (check free-identifier=? (cond-template [#t #'A] [#t #'B]) #'A)
      (check free-identifier=? (cond-template [#f #'A] [#t #'B]) #'B)
      (check free-identifier=? (cond-template [#t #'A] [#t #'B] [else #'C]) #'A)
      (check free-identifier=? (cond-template [#f #'A] [#t #'B] [else #'C]) #'B)
      (check free-identifier=? (cond-template [#f #'A] [#f #'B] [else #'C]) #'C)
      (check bound-identifier=? (cond-template [#t #'A] [#t #'B]) #'A)
      (check bound-identifier=? (cond-template [#f #'A] [#t #'B]) #'B)
      (check bound-identifier=? (cond-template [#t #'A] [#t #'B] [else #'C]) #'A)
      (check bound-identifier=? (cond-template [#f #'A] [#t #'B] [else #'C]) #'B)
      (check bound-identifier=? (cond-template [#f #'A] [#f #'B] [else #'C]) #'C))

    (test-case "when-template"
      (check  free-identifier=? (when-template #t #'A) #'A)
      (check bound-identifier=? (when-template #t #'A) #'A))

    (test-case "unless-template"
      (check  free-identifier=? (unless-template #f #'A) #'A)
      (check bound-identifier=? (unless-template #f #'A) #'A))

    (test-case "for/template"
      (check (curry andmap free-identifier=?)
             (begin-template (list (for/template ([$x (in-syntax #'(A B C))]) #'$x)))
             (list #'A #'B #'C))
      (check (curry andmap bound-identifier=?)
             (begin-template (list (for/template ([$x (in-syntax #'(A B C))]) #'$x)))
             (list #'A #'B #'C)))

    (test-case "for*/template"
      (check (curry andmap free-identifier=?)
             (begin-template (list (for*/template ([$x (in-syntax #'(A B C))]
                                                   [$y (in-range 1 4)])
                                     #'$x$y)))
             (list #'A1 #'A2 #'A3 #'B1 #'B2 #'B3 #'C1 #'C2 #'C3))
      (check (curry andmap bound-identifier=?)
             (begin-template (list (for*/template ([$x (in-syntax #'(A B C))]
                                                   [$y (in-range 1 4)])
                                     #'$x$y)))
             (list #'A1 #'A2 #'A3 #'B1 #'B2 #'B3 #'C1 #'C2 #'C3))))

  (module my-module-template template/lang ($x) #'$x)

  (define-test-suite modules
    (test-case ""
      (load-template tpl 'my-module-template)
      (check free-identifier=? (tpl M) #'M)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (run-all-tests))
