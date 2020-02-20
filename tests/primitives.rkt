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

(require template (for-syntax template))

(module+ test
  (require racket/function
           racket/string
           rackunit
           rackunit/text-ui
           syntax/macro-testing
           (for-syntax racket/base
                       racket/sequence))

  (provide the-tests)

  (define (run-all-tests)
    (run-tests the-tests)
    (void))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-test-suite the-tests
    |expanding environments| |with-template tests| |quote-template tests|)

  (define-syntax X #'A)
  (define-syntax XY #'(A B))
  (define-syntax ZW (list #'A #'B))

  (define-test-suite |expanding environments|
    (test-case "template-expansion time is not macro-expansion time"
      (let-syntax ([z #'Z1])
        (with-template ()
          (let-syntax ([z #'Z2])
            (check eq? '(untemplate (syntax-local-value #'z)) 'Z1)
            (check eq?
                   (let-syntax ([go (λ _ #`'#,(syntax-local-value #'z))]) (go))
                   'Z2))))))

  (define-test-suite |with-template tests|
    (test-case "template variables are resolved"
      (check = (with-template ([$x 1] [$y 2]) $x$y3) 123))
    (test-case "templates are expanded"
      (check eq? (with-template () '(with-template () X)) 'X))
    (test-case "unsyntax escapes to the template-expanding environment"
      (check eq? (with-template () '#,(syntax-local-value #'X)) 'A))
    (test-case "unsyntax-splicing escapes to the template-expanding environment"
      (check equal? (with-template () '(#,@(syntax-local-value #'XY))) '(A B))
      (check equal? (with-template () '(#,@(syntax-local-value #'ZW))) '(A B)))
    (test-case "untemplate escapes to the template-expanding environment"
      (check eq? (with-template () '(untemplate (syntax-local-value #'X))) 'A))
    (test-case "untemplate-splicing escapes to the template-expanding environment"
      (check equal? (with-template () '((untemplate-splicing (syntax-local-value #'XY))))
             '(A B))
      (check equal? (with-template () '((untemplate-splicing (syntax-local-value #'ZW))))
             '(A B)))

    (test-suite "inside quasisyntax"
      (test-case "template variables are resolved"
        (check = (syntax-e (with-template ([$x 1] [$y 2]) #`$x$y3)) 123))
      (test-case "templates are expanded"
        (check eq? (syntax-e (with-template () #`(with-template () X))) 'X))
      (test-case "unsyntax escapes to the macro-expanding environment"
        (check-exn exn:fail:contract?
                   (λ () (with-template () #`#,(syntax-local-value #'X))))
        (check eq? (with-template ()
                     (let-syntax ([go (λ _ #`'#,(syntax-local-value #'X))]) (go)))
               'A))
      (test-case "unsyntax-splicing escapes to the macro-expanding environment"
        (check-exn exn:fail:contract?
                   (λ () (with-template () #`(#,@(syntax-local-value #'XY)))))
        (define x1
          (with-template ()
            (let-syntax ([go (λ _ #`'(#,@(syntax-local-value #'XY)))]) (go))))
        (define x2
          (with-template ()
            (let-syntax ([go (λ _ #`'(#,@(syntax-local-value #'ZW)))]) (go))))
        (check equal? x1 '(A B))
        (check equal? x2 '(A B)))
      (test-case "untemplate escapes to the template-expanding environment"
        (check equal? (syntax-e (with-template () #`(untemplate (syntax-local-value #'X))))
               'A))
      (test-case "untemplate-splicing escapes to the template-expanding environment"
        (define stx1 (with-template () #`((untemplate-splicing (syntax-local-value #'XY)))))
        (define stx2 (with-template () #`((untemplate-splicing (syntax-local-value #'ZW)))))
        (check equal? (syntax->datum stx1) '(A B))
        (check equal? (syntax->datum stx2) '(A B))))

    (test-suite "inside syntax"
      (test-case "template variables are resolved"
        (check = (syntax-e (with-template ([$x 1] [$y 2]) #'$x$y3)) 123))
      (test-case "templates are expanded"
        (check eq? (syntax-e (with-template () #'(with-template () X))) 'X))
      (test-case "unsyntax does not escape to an expanding environment"
        (check equal? (syntax->datum (with-template () #'#,X)) '#,X))
      (test-case "unsyntax-splicing does not escape to an expanding environment"
        (check equal? (syntax->datum (with-template () #'#,@XY)) '#,@XY))
      (test-case "untemplate escapes to the template-expanding environment"
        (check equal? (syntax-e (with-template () #'(untemplate (syntax-local-value #'X))))
               'A))
      (test-case "untemplate-splicing escapes to the template-expanding environment"
        (define stx1 (with-template () #'((untemplate-splicing (syntax-local-value #'XY)))))
        (define stx2 (with-template () #'((untemplate-splicing (syntax-local-value #'ZW)))))
        (check equal? (syntax->datum stx1) '(A B))
        (check equal? (syntax->datum stx2) '(A B)))))

  (define-test-suite |quote-template tests|
    (test-case "template variables are resolved"
      (check = (syntax-e (quote-template ([$x 1] [$y 2]) #'$x$y3)) 123))
    (test-case "templates are not expanded"
      (check equal? (quote-template () '(quote-template () X)) '(quote-template () X)))
    (test-case "unsyntax does not escape to an expanding environment"
      (check equal?
             (quote-template () '#,(syntax-local-value #'X))
             '#,(syntax-local-value #'X)))
    (test-case "unsyntax-splicing does not escape to an expanding environment"
      (check equal?
             (quote-template () '(#,@(syntax-local-value #'XY)))
             '(#,@(syntax-local-value #'XY)))
      (check equal?
             (quote-template () '(#,@(syntax-local-value #'ZW)))
             '(#,@(syntax-local-value #'ZW))))
    (test-case "untemplate does not escape to an expanding environment"
      (check equal?
             (quote-template () '(untemplate (syntax-local-value #'X)))
             '(untemplate (syntax-local-value #'X))))
    (test-case "untemplate-splicing does not escape to an expanding environment"
      (check equal?
             (quote-template () '((untemplate-splicing (syntax-local-value #'XY))))
             '((untemplate-splicing (syntax-local-value #'XY)))))

    (test-suite "inside quasisyntax"
      (test-case "template variables are resolved"
        (check = (syntax-e (quote-template ([$x 1] [$y 2]) #`$x$y3)) 123))
      (test-case "templates are not expanded"
        (check-pred (negate number?)
                    (syntax->datum (quote-template () #`(with-template () 1)))))
      (test-case "unsyntax escapes to the macro-expanding environment"
        (check-exn exn:fail:contract?
                   (λ () (quote-template () #`#,(syntax-local-value #'X))))
        (check eq? (quote-template ()
                     (let-syntax ([go (λ _ #`'#,(syntax-local-value #'X))]) (go)))
               'A))
      (test-case "unsyntax-splicing escapes to the macro-expanding environment"
        (check-exn exn:fail:contract?
                   (λ () (quote-template () #`(#,@(syntax-local-value #'XY)))))
        (define x1
          (quote-template ()
            (let-syntax ([go (λ _ #`'(#,@(syntax-local-value #'XY)))]) (go))))
        (define x2
          (quote-template ()
            (let-syntax ([go (λ _ #`'(#,@(syntax-local-value #'ZW)))]) (go))))
        (check equal? x1 '(A B))
        (check equal? x2 '(A B)))
      (test-case "untemplate does not escape to an expanding environment"
        (check (negate equal?)
               (syntax-e (quote-template () #`(untemplate (syntax-local-value #'X))))
               'A))
      (test-case "untemplate-splicing does not escape to an expanding environment"
        (define stx1 (quote-template () #'((untemplate-splicing (syntax-local-value #'XY)))))
        (define stx2 (quote-template () #'((untemplate-splicing (syntax-local-value #'ZW)))))
        (check (negate equal?) (syntax->datum stx1) '(A B))
        (check (negate equal?) (syntax->datum stx2) '(A B))))

    (test-suite "inside syntax"
      (test-case "template variables are resolved"
        (check = (syntax-e (quote-template ([$x 1] [$y 2]) #'$x$y3)) 123))
      (test-case "templates are not expanded"
        (check-pred (negate number?)
                    (syntax->datum (quote-template () #'(with-template () 1)))))
      (test-case "unsyntax does not escape to an expanding environment"
        (check equal? (syntax->datum (quote-template () #'#,X)) '#,X))
      (test-case "unsyntax-splicing does not escape to an expanding environment"
        (check equal? (syntax->datum (quote-template () #'#,@XY)) '#,@XY))
      (test-case "untemplate does not escape to an expanding environment"
        (check (negate equal?)
               (syntax-e (quote-template () #'(untemplate (syntax-local-value #'X))))
               'A))
      (test-case "untemplate-splicing does not escape to an expanding environment"
        (define stx1 (quote-template () #'((untemplate-splicing (syntax-local-value #'XY)))))
        (define stx2 (quote-template () #'((untemplate-splicing (syntax-local-value #'ZW)))))
        (check (negate equal?) (syntax->datum stx1) '(A B))
        (check (negate equal?) (syntax->datum stx2) '(A B)))))

  (run-all-tests))
