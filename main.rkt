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
                     racket/syntax
                     syntax/parse
                     syntax/parse/define)
         (for-meta 2 racket/base)
         (for-template racket/base))

(provide untemplate untemplate-splicing define-template let-template letrec-template
         splicing-let-template splicing-letrec-template with-template quote-template
         begin-template begin0-template if-template cond-template when-template
         unless-template for/template for*/template template-module-begin load-template
         (for-syntax templates template))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binding Forms

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top-Level Templates

(define-syntax-parser with-template
  [(_ ([var:id arg] ...) tpl ...)
   (define tpl*s (resolve-with (attribute var) (attribute arg) (attribute tpl)))
   #`(begin #,@(null-voided tpl*s))])

(define-syntax-parser quote-template
  [(_ ([var:id arg] ...) tpl ...)
   #:with (arg* ...) (map syntax-local-introduce (attribute arg))
   (define tpl*s (resolve-quote (attribute var) (attribute arg*) (attribute tpl)))
   #`(begin #,@(null-voided tpl*s))])

(define-syntax-parser begin-template
  [(_ tpl ...) #`(begin #,@(null-voided (resolve-with null null (attribute tpl))))])

(define-syntax-parser begin0-template
  [(_ tpl ...) #`(begin0 #,@(resolve-with null null (attribute tpl)))])

(define-syntax-parser if-template
  [(_ test pass-tpl fail-tpl)
   #`(begin #,@(resolve-if (attribute test) (attribute pass-tpl) (attribute fail-tpl)))])

(begin-for-syntax
  (define-syntax-class not-else (pattern (~not (~literal else)))))

(define-syntax-parser cond-template
  [(_ [test:not-else then-tpl ...] ...
      (~optional [(~literal else) else-tpl ...]))
   #`(begin #,@(resolve-cond (attribute test)
                             (attribute then-tpl)
                             (attribute else-tpl)))])

(define-syntax-parser when-template
  [(_ test-expr tpl ...)
   (define tpl*s (resolve-when (attribute test-expr) (attribute tpl)))
   #`(begin #,@(null-voided tpl*s))])

(define-syntax-parser unless-template
  [(_ test-expr tpl ...)
   (define tpl*s (resolve-unless (attribute test-expr) (attribute tpl)))
   #`(begin #,@(null-voided tpl*s))])

(define-syntax-parser for/template
  [(_ ([var:id seq] ...) tpl ...)
   #`(begin #,@(resolve-comprehension #'for/list
                                      (attribute var)
                                      (attribute seq)
                                      (attribute tpl)))])

(define-syntax-parser for*/template
  [(_ ([var:id seq] ...) tpl ...)
   #`(begin #,@(resolve-comprehension #'for*/list
                                      (attribute var)
                                      (attribute seq)
                                      (attribute tpl)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sub-Templates

(define-for-syntax (resolve-syntax-object stx)
  (parameterize ([current-resolvers (cons resolve-syntax (current-resolvers))])
    (with-syntax ([tpl (resolve-one stx)]) #'#'tpl)))

(define-for-syntax (resolve-quasisyntax-object stx)
  (parameterize ([current-resolvers (cons resolve-quasisyntax (current-resolvers))])
    (with-syntax ([tpl (resolve-one stx)]) #'#`tpl)))

(define-for-syntax (resolve-unsyntax-object stx)
  (parameterize ([current-resolvers (cdr (current-resolvers))])
    (with-syntax ([tpl (resolve-one stx)]) #'#,tpl)))

(define-for-syntax (resolve-unsyntax-splicing-object stx)
  (parameterize ([current-resolvers (cdr (current-resolvers))])
    (with-syntax ([tpl (resolve-one stx)]) #'#,@tpl)))

(define-for-syntax (resolve-untemplate stx)
  (resyntax stx (syntax-local-eval (resolve-one stx))))

(define-for-syntax (resolve-untemplate-splicing stx)
  (define results
    (parameterize ([current-resolvers (cdr (current-resolvers))])
      (for/list ([result (in-list (map syntax-local-eval (resolve stx)))])
        (if (list? result) result (syntax-e result)))))
  (map (curry resyntax stx) (flatten results)))

(define-for-syntax (resolve-app stx)
  (resyntax stx (resolve-many (syntax->list stx))))

(define-for-syntax (resolve-with vars args tpls)
  (parameterize ([current-vars vars]
                 [current-args args]
                 [current-quote? #f]
                 [current-resolvers (cons resolve-template (current-resolvers))]
                 [resolve-inside-syntax? #t])
    (resolve-many tpls)))

(define-for-syntax (resolve-quote vars args tpls)
  (parameterize ([current-vars vars]
                 [current-args args]
                 [current-quote? #t]
                 [current-resolvers (cons resolve-template (current-resolvers))]
                 [resolve-inside-syntax? #f])
    (resolve-many tpls)))

(define-for-syntax (resolve-if test pass fail)
  (resolve (if (syntax-local-eval (resolve-one test)) pass fail)))

(define-for-syntax (resolve-cond tests then-tpls else-tpls)
  (resolve-many (or (for/or ([stx (in-list tests)]
                             [tpls (in-list then-tpls)])
                      (and (syntax-local-eval (resolve-one stx)) tpls))
                    else-tpls null)))

(define-for-syntax (resolve-when test tpls)
  (resolve-many (if (syntax-local-eval (resolve-one test)) tpls null)))

(define-for-syntax (resolve-unless test tpls)
  (resolve-many (if (syntax-local-eval (resolve-one test)) null tpls)))

(define-for-syntax (resolve-comprehension for/?-stx vars seqs tpls)
  (with-syntax ([for/? for/?-stx]
                [(var ...) vars]
                [(seq ...) (resolve-many seqs)]
                [(tpl ...) (resolve-many tpls)])
    (with-syntax ([src #'(for/? ([var seq] ...)
                           #`(quote-template ([var #,var] ...) tpl ...))])
      (define-values (ctx bodies) (syntax-local-eval #'(values #'ctx src)))
      (define rescope (make-syntax-delta-introducer ctx #'here))
      (resolve-many (map (curryr rescope 'remove) bodies)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template Constructors

(define-syntax (untemplate stx)
  (raise-syntax-error #f "illegal outside of template macro" stx))

(define-syntax (untemplate-splicing stx)
  (raise-syntax-error #f "illegal outside of template macro" stx))

(begin-for-syntax
  (define-syntax-class vector-t (pattern _ #:when (vector? (syntax-e this-syntax))))
  (define-syntax-class box-t    (pattern _ #:when (box?    (syntax-e this-syntax))))
  (define-syntax-class hash-t   (pattern _ #:when (hash?   (syntax-e this-syntax))))
  (define-syntax-class prefab-t (pattern _ #:when (struct? (syntax-e this-syntax))))
  (define-syntax-class literal
    (pattern (~or :id :boolean :char :keyword :number :regexp :byte-regexp :str :bytes)))
  (define-literal-set template-forms
    (untemplate untemplate-splicing with-template quote-template begin-template
                begin0-template if-template cond-template when-template
                unless-template for/template for*/template))

  (define (null-voided stxs)
     (if (null? stxs) (list #'(void)) stxs))

  (define-simple-macro (either inside-expr not-inside-expr)
    (if (resolve-inside-syntax?) inside-expr not-inside-expr))

  (define-simple-macro (maybe expr)
    #:with this-syntax (datum->syntax this-syntax 'this-syntax)
    (either expr (list (resolve-app this-syntax)))))

(define-for-syntax resolve-template
  (syntax-parser
    #:literals (syntax quasisyntax unsyntax unsyntax-splicing)
    #:literal-sets (template-forms)
    [     (syntax tpl) (list      (resolve-syntax-object (attribute tpl)))]
    [(quasisyntax tpl) (list (resolve-quasisyntax-object (attribute tpl)))]
    [   (unsyntax tpl) (list (resolve-untemplate         (attribute tpl)))]
    [  (unsyntax-splicing tpl)       (resolve-untemplate-splicing (attribute tpl))]
    [(untemplate-splicing tpl)       (resolve-untemplate-splicing (attribute tpl))]
    [(untemplate          tpl) (list (resolve-untemplate          (attribute tpl)))]
    [(with-template ([var:id arg] ...) tpl ...)
     (resolve-with (attribute var) (attribute arg) (attribute tpl))]
    [(quote-template ([var:id arg] ...) tpl ...)
     (resolve-quote (attribute var) (attribute arg) (attribute tpl))]
    [(if-template test pass fail)
     (resolve-if (attribute test) (attribute pass) (attribute fail))]
    [(cond-template [test:not-else then-tpl ...] ...
                    (~optional [(~literal else) else-tpl ...]))
     (resolve-cond (attribute test) (attribute then-tpl) (attribute else-tpl))]
    [  (when-template test tpl ...)   (resolve-when (attribute test) (attribute tpl))]
    [(unless-template test tpl ...) (resolve-unless (attribute test) (attribute tpl))]
    [(for/template ([var:id seq] ...) tpl ...)
     (resolve-comprehension #'for/list
                            (attribute var)
                            (attribute seq)
                            (attribute tpl))]
    [(for*/template ([var:id seq] ...) tpl ...)
     (resolve-comprehension #'for*/list
                            (attribute var)
                            (attribute seq)
                            (attribute tpl))]
    [(_ ...)   (list (resolve-app     this-syntax))]
    [:vector-t (list (resolve-vector  this-syntax))]
    [:box-t    (list (resolve-box     this-syntax))]
    [:hash-t   (list (resolve-hash    this-syntax))]
    [:prefab-t (list (resolve-prefab  this-syntax))]
    [:literal  (list (resolve-literal this-syntax))]
    [_ (list this-syntax)]))

(define-for-syntax resolve-quasisyntax
  (syntax-parser
    #:literals (syntax quasisyntax unsyntax unsyntax-splicing)
    #:literal-sets (template-forms)
    [     (syntax tpl) (list      (resolve-syntax-object (attribute tpl)))]
    [(quasisyntax tpl) (list (resolve-quasisyntax-object (attribute tpl)))]
    [   (unsyntax tpl) (list    (resolve-unsyntax-object (attribute tpl)))]
    [  (unsyntax-splicing tpl) (list (resolve-unsyntax-splicing-object (attribute tpl)))]
    [(untemplate          tpl)    (maybe (list (resolve-untemplate (attribute tpl))))]
    [(untemplate-splicing tpl) (maybe (resolve-untemplate-splicing (attribute tpl)))]
    [(with-template ([var:id arg] ...) tpl ...)
     (maybe (resolve-with (attribute var) (attribute arg) (attribute tpl)))]
    [(quote-template ([var:id arg] ...) tpl ...)
     (maybe (resolve-quote (attribute var) (attribute arg) (attribute tpl)))]
    [(if-template test pass fail)
     (maybe (resolve-if (attribute test) (attribute pass) (attribute fail)))]
    [(cond-template [test:not-else then-tpl ...] ...
                    (~optional [(~literal else) else-tpl ...]))
     (maybe (resolve-cond (attribute test) (attribute then-tpl) (attribute else-tpl)))]
    [  (when-template test tpl ...)   (maybe (resolve-when (attribute test) (attribute tpl)))]
    [(unless-template test tpl ...) (maybe (resolve-unless (attribute test) (attribute tpl)))]
    [(for/template ([var:id seq] ...) tpl ...)
     (maybe (resolve-comprehension #'for/list
                                   (attribute var)
                                   (attribute seq)
                                   (attribute tpl)))]
    [(for*/template ([var:id seq] ...) tpl ...)
     (maybe (resolve-comprehension #'for*/list
                                   (attribute var)
                                   (attribute seq)
                                   (attribute tpl)))]
    [(_ ...)   (list (resolve-app     this-syntax))]
    [:vector-t (list (resolve-vector  this-syntax))]
    [:box-t    (list (resolve-box     this-syntax))]
    [:hash-t   (list (resolve-hash    this-syntax))]
    [:prefab-t (list (resolve-prefab  this-syntax))]
    [:literal  (list (resolve-literal this-syntax))]
    [_ (list this-syntax)]))

(define-for-syntax resolve-syntax
  (syntax-parser
    #:literal-sets (template-forms)
    [(untemplate          tpl)    (maybe (list (resolve-untemplate (attribute tpl))))]
    [(untemplate-splicing tpl) (maybe (resolve-untemplate-splicing (attribute tpl)))]
    [(with-template ([var:id arg] ...) tpl ...)
     (maybe (resolve-with (attribute var) (attribute arg) (attribute tpl)))]
    [(quote-template ([var:id arg] ...) tpl ...)
     (maybe (resolve-quote (attribute var) (attribute arg) (attribute tpl)))]
    [(if-template test pass fail)
     (maybe (resolve-if (attribute test) (attribute pass) (attribute fail)))]
    [(cond-template [test:not-else then-tpl ...] ...
                    (~optional [(~literal else) else-tpl ...]))
     (maybe (resolve-cond (attribute test) (attribute then-tpl) (attribute else-tpl)))]
    [  (when-template test tpl ...)   (maybe (resolve-when (attribute test) (attribute tpl)))]
    [(unless-template test tpl ...) (maybe (resolve-unless (attribute test) (attribute tpl)))]
    [(for/template ([var:id seq] ...) tpl ...)
     (maybe (resolve-comprehension #'for/list
                                   (attribute var)
                                   (attribute seq)
                                   (attribute tpl)))]
    [(for*/template ([var:id seq] ...) tpl ...)
     (maybe (resolve-comprehension #'for*/list
                                   (attribute var)
                                   (attribute seq)
                                   (attribute tpl)))]
    [(_ ...)   (list (resolve-app     this-syntax))]
    [:vector-t (list (resolve-vector  this-syntax))]
    [:box-t    (list (resolve-box     this-syntax))]
    [:hash-t   (list (resolve-hash    this-syntax))]
    [:prefab-t (list (resolve-prefab  this-syntax))]
    [:literal  (list (resolve-literal this-syntax))]
    [_ (list this-syntax)]))

(define-for-syntax current-vars (make-parameter null))
(define-for-syntax current-args (make-parameter null))
(define-for-syntax current-quote? (make-parameter #f))
(define-for-syntax current-resolvers (make-parameter (list resolve-template)))
(define-for-syntax resolve-inside-syntax? (make-parameter #t))

(define-for-syntax (resolve stx)
  ((car (current-resolvers)) stx))

(define-for-syntax (resolve-one stx)
  (define results (resolve stx))
  (cond [(null? results)
         (raise-syntax-error
          #f "no template generated for single-template context" stx)]
        [(null? (cdr results)) (car results)]
        [else
         (raise-syntax-error
          #f "multiple templates generated for single-template context" stx)]))

(define-for-syntax (resolve-many stxs)
  (flatten (map resolve stxs)))

(define-for-syntax (resyntax ctx val)
  (datum->syntax ctx val ctx ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Literal Forms

(define-for-syntax (resolve-vector stx)
  ((resolve-special-syntax stx) (compose list->vector resolve-many vector->list)))

(define-for-syntax (resolve-box stx)
  ((resolve-special-syntax stx) (compose (curry apply box-immutable) resolve unbox)))

(define-for-syntax (resolve-hash stx)
  ((resolve-special-syntax stx)
   (λ (H)
     ((cond [(hash-eq?  H) make-immutable-hasheq ]
            [(hash-eqv? H) make-immutable-hasheqv]
            [else make-immutable-hash])
      (for/list ([(key val) (in-hash H)])
        (cons (syntax->datum (resolve-one (datum->syntax #f key)))
              (resolve-one val)))))))

(define-for-syntax (resolve-prefab stx)
  ((resolve-special-syntax stx)
   (λ (P)
     (define key (resolve-one (prefab-struct-key P)))
     (apply make-prefab-struct
            (syntax->datum (resolve-one (datum->syntax #f key)))
            (resolve-many (struct->list P))))))

(define-for-syntax ((resolve-special-syntax stx) handler)
  (if (has-template-vars? (syntax->string stx))
      (resyntax stx (handler (syntax-e stx)))
      stx))

(define-for-syntax (resolve-literal stx)
  (define str (syntax->string stx))
  (if (has-template-vars? str)
      (string->syntax (if (current-quote?) (syntax-local-introduce (var-arg str)) stx)
                      (resolve-vars str))
      stx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template Variables

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

(define-for-syntax (resolve-vars str)
  (for/fold ([str str])
            ([x (in-list (map syntax->string (current-vars)))]
             [a (in-list (map syntax->string (current-args)))])
    (string-replace str x a)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #lang template

(module reader syntax/module-reader template/lang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module Templates

(define-simple-macro (template-module-begin (var:id ...) tpl ...)
  #:do [(define rescope
          (compose (curryr (make-syntax-delta-introducer #'here #f) 'add)
                   (curryr (make-syntax-delta-introducer this-syntax #f) 'remove)))]
  #:with the-template (datum->syntax this-syntax 'the-template)
  #:with (var* ...) (rescope #'(var ...))
  #:with (((req ...) ...) (tpl* ...))
  (call-with-values
   (λ ()
     (for/fold ([reqs null] [tpls null]) ([stx (in-list (attribute tpl))])
       (syntax-parse (local-expand stx 'top-level (list #'require))
         [((~literal require) spec ...) (values (cons (attribute spec) reqs) tpls)]
         [_ (values reqs (cons this-syntax tpls))])))
   list)
  (#%module-begin
   (require syntax/parse/define template (for-syntax racket/base) (~@ req ...) ...)
   (provide the-template)
   (define-syntax-parser the-template
     [(_ arg (... ...))
      #:when (= (length (attribute arg))
                (length (syntax->list #'(var* ...))))
      #:with (var** (... ...)) #'(var* ...)
      #:with (arg** (... ...)) (attribute arg)
      #:with (tpl** (... ...)) #'(tpl* ...)
      #'(quote-template ([var** arg**] (... ...)) tpl** (... ...))])))

(define-simple-macro (load-template name:id mod-path)
  #:with the-template (datum->syntax this-syntax 'the-template)
  (local-require (rename-in mod-path [the-template name])))
