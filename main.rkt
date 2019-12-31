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
                     syntax/strip-context))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-simple-macro (begin-template ([var:id val] ...) form ...)
  #:do [(define (id->string stx)
          (if (identifier? stx) (symbol->string (syntax-e stx)) stx))
        (define (string->id ctx str)
          (datum->syntax ctx (read (open-input-string str))))
        (define vars (map id->string (attribute var)))
        (define vals (map id->string (attribute val)))
        (define (resolve-template stx)
          (syntax-parse stx
            [:id (resolve-id stx)]
            [(a ...) (datum->syntax stx (map resolve-template (attribute a)))]
            [_ stx]))
        (define (resolve-id stx)
          (define str (id->string stx))
          (if (has-template-vars? str) (string->id stx (resolve-vars str)) stx))
        (define (has-template-vars? str)
          (for/or ([x (in-list vars)]) (string-contains? str x)))
        (define (resolve-vars str)
          (for/fold ([str str])
                    ([x (in-list vars)]
                     [a (in-list vals)])
            (string-replace str x (if (string? a) a (~a (syntax-e a))))))]
  #:with (form* ...) (map resolve-template (attribute form))
  (begin form* ...))

(begin-for-syntax
  (define-simple-macro (template (var:id ...) form ...)
    (λ (stx)
      (syntax-parse stx
        [(_ val (... ...))
         #:with (var* (... ...)) #'(var ...)
         (datum->syntax stx `(begin-template
                               ,(for/list ([x (in-syntax #'(var* (... ...)))]
                                           [a (in-syntax #'(val (... ...)))])
                                  `[,x ,a])
                               form ...))]))))

(define-simple-macro (define-template (name:id var:id ...) form ...)
  (define-syntax name (template (var ...) form ...)))

(define-simple-macro (let-template ([(name:id var:id ...) form ...] ...) body ...)
  (let-syntax ([name (template (var ...) form ...)] ...) body ...))

(define-simple-macro (splicing-let-template ([(name:id var:id ...) form ...] ...) body ...)
  (splicing-let-syntax ([name (template (var ...) form ...)] ...) body ...))

(define-simple-macro (for/template ([var:id seq] ...) form ...)
  #:with (tpl ...) (map (curry replace-context this-syntax)
                        (syntax-local-eval #'(for/list ([var seq] ...)
                                               #`(begin-template ([var #,var] ...) form ...))))
  (begin tpl ...))

(define-simple-macro (define-template-ids set-id:id member-id:id ...)
  (define-syntax set-id #'(member-id ...)))

(define-for-syntax (in-template-ids stx)
  (in-syntax (syntax-local-value stx)))

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
