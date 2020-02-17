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
  (require rackunit rackunit/text-ui syntax/macro-testing
           (for-syntax racket/base))

  (define (run-all-tests)
    (run-tests binding-forms)
    (void))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-test-suite binding-forms
    (test-case "templates"
      (define-syntax tpl (templates [() 0] [($x) $x0] [($x $y) $x00$y00]))
      (check = (tpl) 0)
      (check = (tpl 1) 10)
      (check = (tpl 1 2) 100200))

    (test-case "template"
      (define-syntax tpl (template ($x $y) '($x$y $y$x $xy $yx)))
      (check equal? (tpl a b) '(ab ba ay bx)))

    (test-case "define-template"
      (define-template (tpl $x) (define $xs '($x $x $x $x)))
      (tpl a)
      (check equal? as '(a a a a)))

    (test-case "define-template recursive"
      (define-template (power $b $p)
        (if-template (zero? $p) 1 (* $b (power $b (untemplate (sub1 $p))))))
      (define-template (power* $b $p)
        (if-template (zero? $p) 1 `(* $b ,(power* $b (untemplate (sub1 $p))))))
      (check = (power 2 3) 8)
      (check equal? (power* 2 3) '(* 2 (* 2 (* 2 1))))
      (check-exn #rx"illegal outside of template macro$"
                 (λ () (convert-syntax-error (untemplate 123)))))

    (test-case "let-template"
      (check equal? (let-template ([(foo $x $y) '($x$y $y$x)]
                                   [(bar $x $y) '($xy  $yx )])
                      (append (foo a b) (bar c d)))
             '(ab ba cy dx)))

      (test-case "letrec-template"
        (check-exn #rx"evn: unbound identifier"
                   (λ ()
                     (convert-compile-time-error
                      (let-template ([(evn $b) #t]
                                     [(odd $b) (not (evn $b))])
                        (odd 123)))))
        (letrec-template
            ([(is-even? $n) (if-template (zero? $n) #t (is-odd? (untemplate (sub1 $n))))]
             [(is-odd? $n) (not (is-even? $n))])
          (check-false (is-even? 11))
          (check-true (is-even? 10))))

      (test-case "splicing-let-template"
        (splicing-let-template ([(tpl $x) (define $xs '($x $x $x $x $x))])
          (tpl a))
        (check equal? as '(a a a a a)))

      (test-case "splicing-letrec-template"
        (splicing-letrec-template
            ([(is-even? $n) (if-template (zero? $n) #t (is-odd? (untemplate (sub1 $n))))]
             [(is-odd? $n) (not (is-even? $n))])
          (define is-11-even? (is-even? 11))
          (define is-10-even? (is-even? 10)))
        (check-false is-11-even?)
        (check-true is-10-even?))

    (test-suite "combiners"
      (test-case "if-template"
        (define-template (tpl $b) (if-template $b $b 0))
        (check-true (tpl #t))
        (check = (tpl #f) 0))

      (test-case "cond-template"
        (define-template (tpl $a)
          (cond-template [( number? $a) 'N]
                         [(boolean? $a) 'B]
                         [( string? $a) 'S]
                         [    else      '?]))
        (check eq? (tpl     123) 'N)
        (check eq? (tpl "\"x\"") 'S)
        (check eq? (tpl      #f) 'B)
        (check eq? (tpl     '()) '?))

      (test-case "when-template"
        (define-template (tpl $b) 0 (when-template $b 1))
        (check = (tpl #f) 0)
        (check = (tpl #t) 1))

      (test-case "unless-template"
        (define-template (tpl $b) 0 (unless-template $b 1))
        (check = (tpl #t) 0)
        (check = (tpl #f) 1))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (run-all-tests))
