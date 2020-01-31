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

(require racket/splicing
         syntax/parse/define
         (for-syntax racket/base
                     racket/function
                     racket/list
                     racket/string
                     racket/struct
                     racket/sequence
                     racket/syntax
                     syntax/parse
                     syntax/parse/define)
         (for-meta 2 racket/base))

(provide (all-defined-out)
         (for-syntax templates template in-template-ids))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (untemplate stx)
  (raise-syntax-error #f "illegal outside of template macro" stx))

(define-syntax (untemplate-splicing stx)
  (raise-syntax-error #f "illegal outside of template macro" stx))

(define-for-syntax current-vars (make-parameter #f))
(define-for-syntax current-args (make-parameter #f))
(define-for-syntax current-quote? (make-parameter #f))

(begin-for-syntax
  (define-syntax-class vector-t (pattern _ #:when (vector? (syntax-e this-syntax))))
  (define-syntax-class box-t    (pattern _ #:when (box?    (syntax-e this-syntax))))
  (define-syntax-class hash-t   (pattern _ #:when (hash?   (syntax-e this-syntax))))
  (define-syntax-class prefab-t (pattern _ #:when (struct? (syntax-e this-syntax))))
  (define-syntax-class not-else (pattern (~not (~literal else)))))

(define-for-syntax resolve-template
  (syntax-parser
    #:literals (syntax quasisyntax
                       unsyntax unsyntax-splicing untemplate untemplate-splicing
                       if-template cond-template when-template unless-template
                       for/template for*/template)
    [(syntax tpl) (resolve-syntax-object (attribute tpl))]
    [(quasisyntax tpl) (resolve-quasisyntax (attribute tpl))]
    [((~or unsyntax untemplate) tpl) (resolve-untemplate (attribute tpl))]
    [((~or unsyntax-splicing untemplate-splicing) tpl)
     (resolve-untemplate-splicing (attribute tpl))]
    [(if-template test-expr then-tpl else-tpl)
     (resolve-if resolve-template
                 (attribute test-expr)
                 (attribute then-tpl)
                 (attribute else-tpl))]
    [(cond-template [test-expr:not-else then-tpl ...] ...
                    (~optional [(~literal else) else-tpl ...]))
     (resolve-cond resolve-template
                   (attribute test-expr)
                   (attribute then-tpl)
                   (attribute else-tpl))]
    [(when-template test-expr tpl ...)
     (or (resolve-when resolve-template (attribute test-expr) (attribute tpl))
         null)]
    [(unless-template test-expr tpl ...)
     (or (resolve-unless resolve-template (attribute test-expr) (attribute tpl))
         null)]
    [(for/template ([var:id seq] ...) tpl ...)
     (map syntax-local-introduce
          (resolve-comprehension #'for/list resolve-template
                                 (map syntax-local-introduce (attribute var))
                                 (map syntax-local-introduce (attribute seq))
                                 (map syntax-local-introduce (attribute tpl))))]
    [(for*/template ([var:id seq] ...) tpl ...)
     (map syntax-local-introduce
          (resolve-comprehension #'for*/list resolve-template
                                 (map syntax-local-introduce (attribute var))
                                 (map syntax-local-introduce (attribute seq))
                                 (map syntax-local-introduce (attribute tpl))))]
    [(_ ...) (resolve-app this-syntax)]
    [:vector-t (resolve-vector this-syntax resolve-template)]
    [:box-t    (resolve-box    this-syntax resolve-template)]
    [:hash-t   (resolve-hash   this-syntax resolve-template)]
    [:prefab-t (resolve-prefab this-syntax resolve-template)]
    [(~or :id :boolean :char :keyword :number :regexp :byte-regexp :str :bytes)
     (resolve-literal this-syntax)]
    [_ (list this-syntax)]))

;;; Inside quasisyntax, unsyntax (unsyntax-splicing) does not alias untemplate
;;; (untemplate-splicing). This way, unsyntax in generated code can escape to
;;; the expanding environment of a generated macro.
;;;
;;; For example, if
;;;
;;;   #`#,(syntax-local-value #'x)  ~>  #`(untemplate (syntax-local-value #'x)
;;;
;;; then
;;;
;;;   (let-syntax ([go (λ _ #`(+ 1 #,(syntax-local-value #'x)))]) (go))
;;;   ~>
;;;   (let-syntax ([go (λ _ (#`+ 1 (untemplate-local-value #'x)))]) (go))
;;;
;;; would raise an error if x is not bound in the enclosing template macro.

(define-for-syntax resolve-quasitemplate
  (syntax-parser
    #:literals (untemplate untemplate-splicing if-template cond-template
                           when-template unless-template for/template
                           for*/template)
    [(untemplate tpl) (resolve-untemplate (attribute tpl))]
    [(untemplate-splicing tpl) (resolve-untemplate-splicing (attribute tpl))]
    [(if-template test-expr then-tpl else-tpl)
     (resolve-if resolve-quasitemplate
                 (attribute test-expr)
                 (attribute then-tpl)
                 (attribute else-tpl))]
    [(cond-template [test-expr:not-else then-tpl ...] ...
                    (~optional [(~literal else) else-tpl ...]))
     (resolve-cond resolve-quasitemplate
                   (attribute test-expr)
                   (attribute then-tpl)
                   (attribute else-tpl))]
    [(when-template test-expr tpl ...)
     (or (resolve-when resolve-quasitemplate (attribute test-expr) (attribute tpl))
         null)]
    [(unless-template test-expr tpl ...)
     (or (resolve-unless resolve-quasitemplate (attribute test-expr) (attribute tpl))
         null)]
    [(for/template ([var:id seq] ...) tpl ...)
     (map syntax-local-introduce
          (resolve-comprehension #'for/list resolve-quasitemplate
                                 (map syntax-local-introduce (attribute var))
                                 (map syntax-local-introduce (attribute seq))
                                 (map syntax-local-introduce (attribute tpl))))]
    [(for*/template ([var:id seq] ...) tpl ...)
     (map syntax-local-introduce
          (resolve-comprehension #'for*/list resolve-quasitemplate
                                 (map syntax-local-introduce (attribute var))
                                 (map syntax-local-introduce (attribute seq))
                                 (map syntax-local-introduce (attribute tpl))))]
    [(_ ...) (resolve-quasi-app this-syntax)]
    [:vector-t (resolve-vector this-syntax resolve-quasitemplate)]
    [:box-t    (resolve-box    this-syntax resolve-quasitemplate)]
    [:hash-t   (resolve-hash   this-syntax resolve-quasitemplate)]
    [:prefab-t (resolve-prefab this-syntax resolve-quasitemplate)]
    [(~or :id :boolean :char :keyword :number :regexp :byte-regexp :str :bytes)
     (resolve-literal this-syntax)]
    [_ (list this-syntax)]))

;;; Inside syntax, unsyntax (unsyntax-splicing) does not alias untemplate
;;; (untemplate-splicing). This way, template macros can wrap unsyntax
;;; (unsyntax-splicing) in a syntax object!
;;;
;;; For example, if
;;;
;;;    #'#,x  ~>  #'(untemplate x)
;;;
;;; an error would be raised if x is not bound in the enclosing template
;;; macro.

(define-for-syntax resolve-syntax
  (syntax-parser
    #:literals (syntax unsyntax untemplate untemplate-splicing)
    [(syntax tpl) (resolve-syntax-object (attribute tpl))]
    [(unsyntax tpl) (resolve-unsyntax (attribute tpl))]
    [(untemplate tpl) (resolve-untemplate (attribute tpl))]
    [(untemplate-splicing tpl) (resolve-untemplate-splicing (attribute tpl))]
    [(_ ...) (resolve-syntax-app this-syntax)]
    [:vector-t (resolve-vector this-syntax resolve-syntax)]
    [:box-t    (resolve-box    this-syntax resolve-syntax)]
    [:hash-t   (resolve-hash   this-syntax resolve-syntax)]
    [:prefab-t (resolve-prefab this-syntax resolve-syntax)]
    [(~or :id :boolean :char :keyword :number :regexp :byte-regexp :str :bytes)
     (resolve-literal this-syntax)]
    [_ (list this-syntax)]))

(define-for-syntax ((resolve-many f) stx)
  (list (resyntax stx ((many f) (syntax->list stx)))))

(define-for-syntax ((many f) stxs)
  (flatten (map f stxs)))

(define-for-syntax resolve-app        (resolve-many resolve-template))
(define-for-syntax resolve-quasi-app  (resolve-many resolve-quasitemplate))
(define-for-syntax resolve-syntax-app (resolve-many resolve-syntax))

(define-for-syntax (resolve-quasisyntax stx)
  (with-syntax ([(tpl* ...) (resolve-quasitemplate stx)])
    (syntax->list #'(#`tpl* ...))))

(define-for-syntax (resolve-syntax-object stx)
  (with-syntax ([(tpl* ...) (resolve-syntax stx)])
    (syntax->list #'(#'tpl* ...))))

(define-for-syntax (resolve-unsyntax stx)
  (with-syntax ([(tpl* ...) (resolve-syntax stx)])
    (syntax->list #'(#,tpl* ...))))

(define-for-syntax (resolve-untemplate stx)
  (list (resyntax stx (template-local-eval stx))))

(define-for-syntax (resolve-untemplate-splicing stx)
  (map (curry resyntax stx) (template-local-eval stx)))

(define-for-syntax (resolve-if resolver test-expr then-tpl else-tpl)
  (resolver (if (template-local-eval test-expr) then-tpl else-tpl)))

(define-for-syntax (resolve-cond resolver test-exprs then-tpls else-tpls)
  ((many resolver)
   (or (for/or ([stx  (in-list test-exprs)]
                [tpls (in-list then-tpls)])
         (and (template-local-eval stx) tpls))
       else-tpls
       null)))

(define-for-syntax (resolve-when resolver test-expr tpls)
  (define tpls*
    ((many resolver) (if (template-local-eval test-expr) tpls null)))
  (if (null? tpls*) #f tpls*))

(define-for-syntax (resolve-unless resolver test-expr tpls)
  ((many resolver) (if (template-local-eval test-expr) null tpls)))

(define-for-syntax (resolve-comprehension for/?-stx resolver vars seqs tpls)
  (with-syntax ([for/? for/?-stx]
                [(var ...) vars]
                [(seq ...) seqs]
                [(tpl ...) tpls])
    (with-syntax ([src #'(for/? ([var seq] ...)
                           #`(with-template ([var #,var] ...) tpl ...))])
      (define-values (ctx bodies) (syntax-local-eval #'(values #'ctx src)))
      (define rescope (make-syntax-delta-introducer ctx #'here))
      (for*/list ([body  (in-list bodies)]
                  [body* (in-list (resolver (rescope body 'remove)))])
        (local-expand body* 'top-level #f)))))

(define-for-syntax (resolve-vector stx resolver)
  ((resolve-special stx) (compose list->vector (many resolver) vector->list)))

(define-for-syntax (resolve-box stx [resolver resolve-template])
  ((resolve-special stx) (compose (curry apply box-immutable) resolver unbox)))

(define-for-syntax (resolve-hash stx [resolver resolve-template])
  ((resolve-special stx)
   (λ (H)
     ((cond [(hash-eq?  H) make-immutable-hasheq ]
            [(hash-eqv? H) make-immutable-hasheqv]
            [else make-immutable-hash])
      (for/list ([(key val) (in-hash H)])
        (cons (syntax->datum (car (resolver (datum->syntax #f key))))
              (car (resolver val))))))))

(define-for-syntax (resolve-prefab stx [resolver resolve-template])
  ((resolve-special stx)
   (λ (P)
     (define key (prefab-struct-key P))
     (apply make-prefab-struct
            (syntax->datum (car (resolver (datum->syntax #f key))))
            ((many resolver) (struct->list P))))))

(define-for-syntax ((resolve-special stx) handle)
  (if (has-template-vars? (syntax->string stx))
      (list (resyntax stx (handle (syntax-e stx))))
      (list stx)))

(define-for-syntax (resolve-literal stx)
  (define str (syntax->string stx))
  (if (has-template-vars? str)
      (list (string->syntax (if (current-quote?) (var-arg str) stx)
                            (resolve-vars str)))
      (list stx)))

(define-for-syntax (resolve-vars str)
  (for/fold ([str str])
            ([x (in-list (map syntax->string (current-vars)))]
             [a (in-list (map syntax->string (current-args)))])
    (string-replace str x a)))

(define-for-syntax (has-template-vars? str)
  (and (current-vars)
       (for/or ([var (in-list (current-vars))])
         (and (has-template-var? str var) var))))

(define-for-syntax (has-template-var? str var)
  (string-contains? str (syntax->string var)))

(define-for-syntax (var-arg str)
  (and (current-vars)
       (for/or ([var (in-list (current-vars))]
                [arg (in-list (current-args))])
         (and (has-template-var? str var) arg))))

(define-for-syntax syntax->string
  (syntax-parser
    [(a ...) (format "(~a)" (string-join (map syntax->string (attribute a))))]
    [:id (format "~a" (syntax->datum this-syntax))]
    [_ (format "~s" (syntax->datum this-syntax))]))

(define-for-syntax (string->syntax ctx str)
  (define port (open-input-string str))
  (begin0 (resyntax ctx (read port))
    (unless (eof-object? (read port))
      (raise-syntax-error
       #f "multiple expressions generated for single-expression context" ctx))))

(define-for-syntax (resyntax ctx val)
  (datum->syntax ctx val ctx ctx))

(define-for-syntax (template-local-eval stx)
  (syntax-local-eval (car (resolve-template stx))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (with-template ([var:id arg] ...) tpl ...)
  #:with ((tpl* ...) ...) (parameterize ([current-vars (attribute var)]
                                         [current-args (attribute arg)])
                            (map resolve-template (attribute tpl)))
  (begin (~@ tpl* ...) ...))

(define-simple-macro (quote-template ([var:id arg] ...) tpl ...)
  #:with ((tpl* ...) ...) (parameterize ([current-vars (attribute var)]
                                         [current-args (attribute arg)]
                                         [current-quote? #t])
                            (map resolve-template (attribute tpl)))
  (begin (~@ tpl* ...) ...))

(begin-for-syntax
  (define-simple-macro (templates [(var:id ...) tpl ...] ...)
    (syntax-parser
      [(_ arg (... ...))
       #:when (= (length (attribute arg))
                 (length (syntax->list #'(var ...))))
       #:with (var* (... ...)) (map syntax-local-introduce (syntax->list #'(var ...)))
       #:with (tpl* (... ...)) (map syntax-local-introduce (syntax->list #'(tpl ...)))
       #'(with-template ([var* arg] (... ...)) tpl* (... ...))]
      ...))

  (define-simple-macro (template (var:id ...) tpl ...)
    (templates [(var ...) tpl ...])))

(define-simple-macro (begin-template tpl ...)
  (with-template () tpl ...))

(define-simple-macro (begin0-template tpl ...)
  (with-template () (begin0 tpl ...)))

(define-simple-macro (define-template (name:id var:id ...) tpl ...)
  (define-syntax name (template (var ...) tpl ...)))

(define-simple-macro (let-template ([(name:id var:id ...) tpl ...] ...) body ...)
  (let-syntax ([name (template (var ...) tpl ...)] ...) body ...))

(define-simple-macro (letrec-template ([(name:id var:id ...) tpl ...] ...) body ...)
  (letrec-syntax ([name (template (var ...) tpl ...)] ...) body ...))

(define-simple-macro (splicing-let-template ([(name:id var:id ...) tpl ...] ...) body ...)
  (splicing-let-syntax ([name (template (var ...) tpl ...)] ...) body ...))

(define-simple-macro (splicing-letrec-template ([(name:id var:id ...) tpl ...] ...) body ...)
  (splicing-letrec-syntax ([name (template (var ...) tpl ...)] ...) body ...))

(define-syntax-parser if-template
  [(_ test-expr then-tpl else-tpl)
   #`(begin #,@(resolve-if resolve-template
                           (attribute test-expr)
                           (attribute then-tpl)
                           (attribute else-tpl)))])

(define-syntax-parser cond-template
  [(_ [test-expr:not-else then-tpl ...] ...
      (~optional [(~literal else) else-tpl ...]))
   #`(begin #,@(resolve-cond resolve-template
                             (attribute test-expr)
                             (attribute then-tpl)
                             (attribute else-tpl)))])

(define-syntax-parser when-template
  [(_ test-expr tpl ...)
   (define tpls*
     (resolve-when resolve-template (attribute test-expr) (attribute tpl)))
   #`(begin #,@(or tpls* (list #'(void))))])

(define-syntax-parser unless-template
  [(_ test-expr tpl ...)
   (define tpls*
     (resolve-unless resolve-template (attribute test-expr) (attribute tpl)))
   #`(begin #,@(or tpls* (list #'(void))))])

(define-simple-macro (for/template ([var:id seq] ...) tpl ...)
  #:with ((_ tpl* ...) ...) (map syntax-local-introduce
                                 (resolve-comprehension
                                  #'for/list resolve-template
                                  (map syntax-local-introduce (attribute var))
                                  (map syntax-local-introduce (attribute seq))
                                  (map syntax-local-introduce (attribute tpl))))
  (begin (~@ tpl* ...) ...))

(define-simple-macro (for*/template ([var:id seq] ...) tpl ...)
  #:with ((_ tpl* ...) ...) (map syntax-local-introduce
                                 (resolve-comprehension
                                  #'for*/list resolve-template
                                  (map syntax-local-introduce (attribute var))
                                  (map syntax-local-introduce (attribute seq))
                                  (map syntax-local-introduce (attribute tpl))))
  (begin (~@ tpl* ...) ...))

(define-simple-macro (define-template-ids set-id:id member-id:id ...)
  (define-syntax set-id #'(member-id ...)))

(begin-for-syntax
  (define-simple-macro (in-template-ids set-id:id)
    (in-syntax (syntax-local-value #'set-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/base)

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [template-module-begin #%module-begin]))

(module reader syntax/module-reader template)

(define-syntax-parser template-module-begin
  [(_ (var:id ...) tpl ...)
   #:with the-template (datum->syntax this-syntax 'the-template)
   #`(#%module-begin
      (require syntax/parse/define template (for-syntax racket/base))
      (provide the-template)
      (define-syntax-parser the-template
        [(_ arg (... ...))
         #:when (= (length (attribute arg))
                   (length (syntax->list #'(var ...))))
         #:with (var* (... ...)) (syntax-local-introduce #'(var ...))
         #'(quote-template ([var* arg] (... ...)) tpl ...)]))])

(define-simple-macro (load-template-module name:id mod-path)
  #:with the-template (datum->syntax this-syntax 'the-template)
  (local-require (rename-in mod-path [the-template name])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit
           syntax/macro-testing
           (for-syntax racket/base))

  (test-case "with-template form"
    (with-template ([$x 1] [$y 2]) (define x $x0$y0))
    (check = x 1020))

  (test-case "with-template expr"
    (check = (with-template ([$x 1] [$y 3]) 123 $x0$y0) 1030))

  (test-case "literal data"
    ;; no-op
    (begin-template)
    ;; no vars defined or used
    (check = (begin-template 123) 123)
    (check eq? (begin-template 'x) 'x)
    (check string=? (begin-template "abc") "abc")
    (check equal? (begin-template '#:jkl) '#:jkl)
    (check equal? (begin-template #(1 2 3)) #(1 2 3))
    (check equal? (begin-template #&x) (box 'x))
    (check equal? (begin-template #hash([x . 1] [y . 2])) #hash([x . 1] [y . 2]))
    (check equal? (begin-template #s(foo f g h)) #s(foo f g h))
    ;; vars defined but not used
    (with-template ([$x A]))
    (check = (with-template ([$x 1]) 123) 123)
    (check eq? (with-template ([$x A]) 'x) 'x)
    (check string=? (with-template ([$x A]) "abc") "abc")
    (check equal? (with-template ([$x J]) '#:jkl) '#:jkl)
    (check equal? (with-template ([$x A]) #(1 2 3)) #(1 2 3))
    (check equal? (with-template ([$x B]) #&x) (box 'x))
    (check equal? (with-template ([$x a] [$y b]) #hash([x . 1] [2 . y]))
           #hash([x . 1] [2 . y]))
    (check equal? (with-template ([$f F] [$h H]) #s(foo f g h)) #s(foo f g h))
    ;; vars defined and used
    (check = (with-template ([$x 1]) $x23) 123)
    (check eq? (with-template ([$x A]) '$x) 'A)
    (check string=? (with-template ([$x A]) "$xbc") "Abc")
    (check equal? (with-template ([$x J]) '#:$xkl) '#:Jkl)
    (check equal? (with-template ([$x A]) #($x 2 3)) #(A 2 3))
    (check equal? (with-template ([$x B]) #&$x) #&B)
    (check equal? (with-template ([$x a] [$y b]) #hash([$x . 1] [2 . $y]))
           #hash([a . 1] [2 . b]))
    (check equal? (with-template ([$f F] [$h H]) #s(foo $f g $h)) #s(foo F g H)))

  (test-case "untemplate"
    (check = (begin-template (untemplate (+ 1 2))) 3)
    (check = (begin-template '(untemplate (* 1 2))) 2)
    (check equal? (with-template ([$x a]) '($x (untemplate '$x))) '(a a))
    (check-exn exn:fail:syntax? (λ () (convert-syntax-error (untemplate 123)))))

  (test-case "untemplate"
    (define-template (power $b $p)
      (if-template (zero? $p) 1 (* $b (power $b (untemplate (sub1 $p))))))
    (define-template (power* $b $p)
      (if-template (zero? $p) 1 `(* $b ,(power* $b (untemplate (sub1 $p))))))
    (check = (power 2 3) 8)
    (check equal? (power* 2 3) '(* 2 (* 2 (* 2 1))))
    (check-exn exn:fail:syntax? (λ () (convert-syntax-error (untemplate 123)))))

  (test-case "untemplate-splicing"
    (check equal? (begin-template '(1 (untemplate-splicing '(2 3)))) '(1 2 3))
    (check equal? (with-template ([$x a]) '($x (untemplate-splicing '($x $x)))) '(a a a))
    (check-exn exn:fail:syntax? (λ () (convert-syntax-error (untemplate-splicing 123)))))

  (test-case "syntax"
    (check-pred syntax? (begin-template #'A))
    (check = (syntax-e (begin-template #'123)) 123)
    (check equal? (syntax->datum (begin-template #'#,A)) '#,A))

  (test-case "unsyntax"
    (let-syntax ([x #'(+ 2 3)])
      (check = (begin-template (+ 1 #,(syntax-local-value #'x))) 6))
    (check equal? (syntax->datum (begin-template #`(+ 1 #,#'(+ 2 3)))) '(+ 1 (+ 2 3))))

  (test-case "quasisyntax"
    (check eq? (syntax->datum (with-template ([$x a]) #`$x)) 'a)
    (check string=? (syntax->datum (with-template ([$x a]) #`"$x $x $x")) "a a a")
    (check eq? (syntax->datum (with-template ([$x a]) #`#,'$x)) 'a)
    (check eq? (syntax->datum (with-template ([$x a]) #`(untemplate '$x))) 'a)
    (check equal? (syntax->datum (with-template ([$x a]) #`($x $x $x))) '(a a a))
    (check = (syntax->datum (with-template ([$x 1]) #`10)) 10))

  (test-case "templates"
    (define-syntax tpl (templates [() 0] [($x) $x0] [($x $y) $x00$y00]))
    (check = (tpl) 0)
    (check = (tpl 1) 10)
    (check = (tpl 1 2) 100200))

  (test-case "template"
    (define-syntax tpl (template ($x $y) '($x$y $y$x $xy $yx)))
    (check equal? (tpl a b) '(ab ba ay bx)))

  (test-case "begin-template form"
    (begin-template (define x 123))
    (check = x 123))

  (test-case "begin-template expr"
    (check = (begin-template 123 456) 456))

  (test-case "begin0-template"
    (check = (begin0-template 123 456) 123))

  (test-case "define-template"
    (define-template (tpl $x) (define $xs '($x $x $x $x)))
    (tpl a)
    (check equal? as '(a a a a)))

  (test-case "let-template"
    (check equal? (let-template ([(foo $x $y) '($x$y $y$x)]
                                 [(bar $x $y) '($xy $yx)])
                    (append (foo a b) (bar c d)))
           '(ab ba cy dx)))

  (test-case "letrec-template"
    (check-exn exn:fail:syntax?
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
    (splicing-let-template ([(tpl $x) (define $xs '($x $x $x $x $x))]) (tpl a))
    (check equal? as '(a a a a a)))

  (test-case "splicing-letrec-template"
    (splicing-letrec-template
        ([(is-even? $n) (if-template (zero? $n) #t (is-odd? (untemplate (sub1 $n))))]
         [(is-odd? $n) (not (is-even? $n))])
      (define is-11-even? (is-even? 11))
      (define is-10-even? (is-even? 10)))
    (check-false is-11-even?)
    (check-true is-10-even?))

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
    (check eq? (tpl 123) 'N)
    (check eq? (tpl "\"x\"") 'S)
    (check eq? (tpl #f) 'B)
    (check eq? (tpl '()) '?))

  (test-case "when-template"
    (define-template (tpl $b) 0 (when-template $b 1))
    (check = (tpl #f) 0)
    (check = (tpl #t) 1))

  (test-case "unless-template"
    (define-template (tpl $b) 0 (unless-template $b 1))
    (check = (tpl #t) 0)
    (check = (tpl #f) 1))

  (test-case "for/template"
    (for/template ([$x (in-syntax #'(A B C))]
                   [$a (in-naturals)])
      (define $x $a0))
    (check = A  0)
    (check = B 10)
    (check = C 20)
    (check equal? (begin-template
                    (list (for/template ([$m (in-range 3)]
                                         [$n (in-range 3)])
                            (+ $n (* $m 3)))))
           '(0 4 8)))

  (test-case "for*/template"
    (for*/template ([$x (in-syntax #'(A B C))]
                    [$y (in-range 3)])
      (define $x$y (add1 $y)))
    (check equal? (list A0 A1 A2 B0 B1 B2 C0 C1 C2) '(1 2 3 1 2 3 1 2 3))
    (check equal? (begin-template
                    (list (for*/template ([$m (in-range 3)]
                                          [$n (in-range 3)])
                            (+ $n (* $m 3)))))
           '(0 1 2 3 4 5 6 7 8)))

  (test-case "*-template-ids"
    (define-template-ids ops + - * /)
    (for/template ([$op (in-template-ids ops)]
                   [$want (in-list '(6 -4 6 1/6))])
      (check-true (= ($op 1 2 3) $want)))))
