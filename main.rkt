#lang racket/base

(require racket/splicing
         syntax/parse/define
         (for-syntax racket/base
                     racket/format
                     racket/function
                     racket/sequence
                     racket/string
                     racket/syntax
                     syntax/parse/define
                     syntax/strip-context)
         (for-meta 2 racket/base))

(provide (all-defined-out)
         (for-syntax template templates in-template-ids))

(define-syntax (untemplate stx)
  (raise-syntax-error #f "illegal outside of template" stx))

(begin-for-syntax
  (define-simple-macro (templates [(var:id ...) form ...] ...)
    (λ (stx)
      (syntax-parse stx
        [(_ val (... ...))
         #:when (= (length (attribute val))
                   (length (syntax->list #'(var ...))))
         #:with (var* (... ...)) #'(var ...)
         (datum->syntax stx `(begin-template
                                 ,(for/list ([x (in-syntax #'(var* (... ...)))]
                                             [a (in-syntax #'(val (... ...)))])
                                    `[,x ,a])
                               form ...))]
        ...)))

  (define-simple-macro (template (var:id ...) form ...)
    (templates [(var ...) form ...]))

  (define current-vars (make-parameter #f))
  (define current-vals (make-parameter #f))

  (define (id->string stx)
    (if (identifier? stx) (symbol->string (syntax-e stx)) stx))

  (define (string->id ctx str)
    (datum->syntax ctx (read (open-input-string str))))

  (define (resolve-template stx)
    (syntax-parse stx
      [:id (resolve-id stx)]
      [((~literal untemplate) expr)
       (datum->syntax stx (syntax-local-eval (resolve-template #'expr)))]
      [(a ...) (datum->syntax stx (map resolve-template (attribute a)))]
      [_ stx]))

  (define (resolve-id stx)
    (define str (id->string stx))
    (if (has-template-vars? str) (string->id stx (resolve-vars str)) stx))

  (define (has-template-vars? str)
    (for/or ([x (in-list (current-vars))]) (string-contains? str x)))

  (define (resolve-vars str)
    (for/fold ([str str])
              ([x (in-list (current-vars))]
               [a (in-list (current-vals))])
      (string-replace str x (if (string? a) a (~a (syntax->datum a)))))))

(define-simple-macro (begin-template ([var:id val] ...) form ...)
  #:with (form* ...) (parameterize
                         ([current-vars (map id->string (attribute var))]
                          [current-vals (map id->string (attribute val))])
                       (map resolve-template (attribute form)))
  (begin form* ...))

(define-simple-macro (begin0-template ([var:id val] ...) expr ...)
  #:with (expr* ...) (parameterize
                         ([current-vars (map id->string (attribute var))]
                          [current-vals (map id->string (attribute val))])
                       (map resolve-template (attribute expr)))
  (begin0 expr* ...))

(define-simple-macro (define-template (name:id var:id ...) form ...)
  (define-syntax name (template (var ...) form ...)))

(define-simple-macro (let-template ([(name:id var:id ...) form ...] ...) body ...)
  (let-syntax ([name (template (var ...) form ...)] ...) body ...))

(define-simple-macro (letrec-template ([(name:id var:id ...) form ...] ...) body ...)
  (letrec-syntax ([name (template (var ...) form ...)] ...) body ...))

(define-simple-macro (splicing-let-template ([(name:id var:id ...) form ...] ...) body ...)
  (splicing-let-syntax ([name (template (var ...) form ...)] ...) body ...))

(define-simple-macro (splicing-letrec-template ([(name:id var:id ...) form ...] ...) body ...)
  (splicing-letrec-syntax ([name (template (var ...) form ...)] ...) body ...))

(define-simple-macro (if-template test pass fail)
  (let-syntax ([go (if test (λ _ #`pass) (λ _ #`fail))]) (go)))

(define-simple-macro (cond-template [(~and test (~not (~literal else))) pass ...]
                                    ...
                                    (~optional [(~literal else) fail ...]))
  (let-syntax ([go (cond [test (λ _ #'(begin pass ...))]
                         ...
                         [else (λ _ (~? #'(begin fail ...) #'(void)))])])
    (go)))

(define-simple-macro (when-template test body ...)
  (let-syntax ([go (if test (λ _ #'(begin body ...)) (λ _ #'(void)))]) (go)))

(define-simple-macro (unless-template test body ...)
  (let-syntax ([go (if test (λ _ #'(void)) (λ _ #'(begin body ...)))]) (go)))

(define-simple-macro (for/template ([var:id seq] ...) form ...)
  #:with (tpl ...) (map (curry replace-context this-syntax)
                        (syntax-local-eval
                         #'(for/list ([var seq] ...)
                             #`(begin-template ([var #,var] ...) form ...))))
  (begin tpl ...))

(define-simple-macro (for*/template ([var:id seq] ...) form ...)
  #:with (tpl ...) (map (curry replace-context this-syntax)
                        (syntax-local-eval
                         #'(for*/list ([var seq] ...)
                             #`(begin-template ([var #,var] ...) form ...))))
  (begin tpl ...))

(define-simple-macro (define-template-ids set-id:id member-id:id ...)
  (define-syntax set-id #'(member-id ...)))

(define-for-syntax (in-template-ids stx)
  (in-syntax (syntax-local-value stx)))

;; (define-template (power $base $exponent)
;;   (if-template (zero? $exponent)
;;                1
;;                `(* $base ,(power $base (untemplate (sub1 $exponent))))))

;; (power 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The #lang

;; (require racket/base
;;          (for-syntax racket/base))

;; (provide (except-out (all-from-out racket/base) y#%module-begin)
;; (for-syntax (all-from-out racket/base)) (rename-out [template-module-begin
;; #%module-begin]))

;; (module reader syntax/module-reader template)

;; (define-simple-macro
;;   (template-module-begin lang:id (~optional (~seq #:vars (var:id ...))) form ...)
;;   #:with (x ...) (or (attribute var) #'())
;;   #:with (arg ...) (generate-temporaries #'(x ...))
;;   #:with instantiate (datum->syntax this-syntax 'instantiate)
;;   (#%module-begin
;;    (define-syntax-parser instantiate
;;      [(_ arg ...)
;;       #:with (y (... ...)) #'(x ...)
;;       #:with (a (... ...)) #'(arg ...)
;;       #:with mod-name #`#,(gensym 'template)
;;       #'(begin-template ([y a] (... ...))
;;           (module mod-name lang form ...)
;;           (require 'mod-name))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unit Tests

(module+ test
  (require rackunit
           syntax/macro-testing)

  (test-case "template"
    (define-syntax tpl (template ($x $y) '($x$y $y$x $xy $yx)))
    (check equal? (tpl a b) '(ab ba ay bx)))

  (test-case "template define"
    (define-syntax tpl (template ($x) (define $xs '($x $x $x))))
    (check-exn exn:fail:syntax? (λ () (convert-syntax-error (tpl a))))
    (tpl a)
    (check equal? as '(a a a)))

  (test-case "define-template"
    (define-template (tpl $x) (define $xs '($x $x $x $x)))
    (tpl a)
    (check equal? as '(a a a a)))

  (test-case "let-template"
    (check equal?
           (let-template ([(foo $x $y) '($x$y $y$x)]
                          [(bar $x $y) '($xy $yx)])
             (append (foo a b) (bar c d)))
           '(ab ba cy dx)))

  (test-case "splicing-let-template"
    (splicing-let-template ([(tpl $x) (define $xs '($x $x $x $x $x))]) (tpl a))
    (check equal? as '(a a a a a)))

  (test-case "for/template"
    (for/template ([$x (in-syntax #'(A B C))]
                   [$a (in-naturals)])
      (define $x $a0))
    (check = A  0)
    (check = B 10)
    (check = C 20)))
