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

(require template)

(module+ test
  (require rackunit rackunit/text-ui
           (for-syntax racket/base racket/sequence))

  (define (run-all-tests)
    (run-tests combiners)
    (void))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-test-suite combiners
    (test-suite "begin-template"
      (test-case "as form" (begin-template (define x 123)) (check = x 123))
      (test-case "as expression" (check = (begin-template 123 456) 456))
      (test-case "inside with-template"
        (check equal? (begin-template `((with-template ([$x 1]) $x #,(+ $x 1) #,(+ $x 2))))
               '(1 2 3))
        (check equal? (begin-template '((with-template ([$x 1]) $x #,(+ $x 1) #,(+ $x 2))))
               '(1 2 3))
        (check equal? (begin-template '((with-template ([$x 1]) $x #,(+ $x 1) #,(+ $x 2))))
               '(1 2 3))))

    (test-case "begin0-template"
      (check = (begin0-template 123 456) 123))

    (test-case "if-template"
      (check equal? (begin-template '((with-template ([$x 1] [$y 0])
                                        (if-template #t $x $y)
                                        (if-template #f $x $y))))
             '(1 0)))

    (test-case "cond-template"
      (check = (cond-template [#t 0] [#f 1] [else -1]) 0)
      (check = (cond-template [#f 0] [#t 1] [else -1]) 1)
      (check = (cond-template [#f 0] [#f 1] [else -1]) -1))

    (test-case "when-template"
      (check-pred void? (when-template #f 1))
      (check-pred void? (with-template ([$b #f]) (when-template $b 1)))
      (check = (when-template #t 1) 1)
      (check = (with-template ([$b #t]) (when-template $b 1)) 1))

    (test-case "unless-template"
      (check-pred void? (unless-template #t 1))
      (check-pred void? (with-template ([$b #t]) (unless-template $b 1)))
      (check = (unless-template #f 1) 1)
      (check = (with-template ([$b #f]) (unless-template $b 1)) 1))

    (test-case "for/template"
      (for/template ([$x (in-syntax #'(A B C))]
                     [$a (in-naturals)])
        (define $x $a0))
      (check = A  0)
      (check = B 10)
      (check = C 20)
      (check equal? (begin-template (list (for/template ([$m (in-range 3)]
                                                         [$n (in-range 3)])
                                            (+ $n (* $m 3)))))
             '(0 4 8)))

    (test-case "for*/template"
      (for*/template ([$x (in-syntax #'(A B C))]
                      [$y (in-range 3)])
        (define $x$y (add1 $y)))
      (check equal? (list A0 A1 A2 B0 B1 B2 C0 C1 C2) '(1 2 3 1 2 3 1 2 3))
      (check equal? (begin-template (list (for*/template ([$m (in-range 3)]
                                                          [$n (in-range 3)])
                                            (+ $n (* $m 3)))))
             '(0 1 2 3 4 5 6 7 8))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (run-all-tests))
