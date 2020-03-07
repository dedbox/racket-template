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

(require debug-scopes
         racket/pretty
         racket/splicing
         syntax/parse/define
         (for-syntax debug-scopes
                     racket/base
                     racket/function
                     racket/list
                     racket/pretty
                     racket/string
                     racket/struct
                     racket/syntax
                     syntax/parse
                     syntax/parse/define)
         (for-meta 2 racket/base)
         (for-template racket/base))

(provide untemplate untemplate-splicing define-templates define-template let-template
         letrec-template splicing-let-template splicing-letrec-template with-template
         semiwith-template quote-template semiquote-template begin-template
         begin0-template if-template cond-template when-template unless-template
         for/template for*/template template-module-begin load-template debug-template
         debug-template/scopes reset-debug-scopes
         (for-syntax templates template semitemplates semitemplate quoted-templates
                     quoted-template semiquoted-templates semiquoted-template))

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
       #'(with-template ([var* arg] (... ...)) (begin-template tpl* (... ...)))]
      ...))

  (define-simple-macro (template (var:id ...) tpl ...)
    (templates [(var ...) tpl ...])))

(begin-for-syntax
  (define-simple-macro (semitemplates [(var:id ...) tpl ...] ...)
    (syntax-parser
      [(_ arg (... ...))
       #:when (= (length (attribute arg))
                 (length (syntax->list #'(var ...))))
       #:with (var* (... ...)) (map syntax-local-introduce (syntax->list #'(var ...)))
       #:with (tpl* (... ...)) (map syntax-local-introduce (syntax->list #'(tpl ...)))
       #'(semiwith-template ([var* arg] (... ...)) (begin-template tpl* (... ...)))]
      ...))

  (define-simple-macro (semitemplate (var:id ...) tpl ...)
    (semitemplates [(var ...) tpl ...])))

(begin-for-syntax
  (define-simple-macro (quoted-templates [(var:id ...) tpl ...] ...)
    (syntax-parser
      [(_ arg (... ...))
       #:when (= (length (attribute arg))
                 (length (syntax->list #'(var ...))))
       #:with (var* (... ...)) (map syntax-local-introduce (syntax->list #'(var ...)))
       #:with (tpl* (... ...)) (map syntax-local-introduce (syntax->list #'(tpl ...)))
       #'(quote-template ([var* arg] (... ...)) (begin-template tpl* (... ...)))]
      ...))

  (define-simple-macro (quoted-template (var:id ...) tpl ...)
    (quoted-templates [(var ...) tpl ...])))

(begin-for-syntax
  (define-simple-macro (semiquoted-templates [(var:id ...) tpl ...] ...)
    (syntax-parser
      [(_ arg (... ...))
       #:when (= (length (attribute arg))
                 (length (syntax->list #'(var ...))))
       #:with (var* (... ...)) (map syntax-local-introduce (syntax->list #'(var ...)))
       #:with (tpl* (... ...)) (map syntax-local-introduce (syntax->list #'(tpl ...)))
       #'(semiquote-template ([var* arg] (... ...)) (begin-template tpl* (... ...)))]
      ...))

  (define-simple-macro (semiquoted-template (var:id ...) tpl ...)
    (semiquoted-templates [(var ...) tpl ...])))

(define-simple-macro (define-template (name:id var:id ...) tpl ...)
  (define-syntax name (template (var ...) tpl ...)))

(define-simple-macro (define-templates [(name:id var:id ...) tpl ...] ...)
  (begin (define-template (name var ...) tpl ...) ...))

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
   (syntax-local-introduce
    (null-voided
     #'begin
     (resolve-with (map syntax-local-introduce (attribute var))
                   (attribute arg)
                   (map syntax-local-introduce (attribute tpl)))))])

(define-syntax-parser semiwith-template
  [(_ ([var:id arg] ...) tpl ...)
   (syntax-local-introduce
    (null-voided
     #'begin
     (resolve-semiwith (map syntax-local-introduce (attribute var))
                       (attribute arg)
                       (map syntax-local-introduce (attribute tpl)))))])

(define-syntax-parser quote-template
  [(_ ([var:id arg] ...) tpl ...)
   (syntax-local-introduce
    (null-voided
     #'begin
     (resolve-quote (map syntax-local-introduce (attribute var))
                    (attribute arg)
                    (map syntax-local-introduce (attribute tpl)))))])

(define-syntax-parser semiquote-template
  [(_ ([var:id arg] ...) tpl ...)
   (syntax-local-introduce
    (null-voided
     #'begin
     (resolve-semiquote (map syntax-local-introduce (attribute var))
                        (attribute arg)
                        (map syntax-local-introduce (attribute tpl)))))])

(define-syntax-parser begin-template
  [(_ tpl ...)
   (syntax-local-introduce
    (null-voided
     #'begin
     (resolve-with null null (map syntax-local-introduce (attribute tpl)))))])

(define-syntax-parser begin0-template
  [(_ tpl ...)
   (syntax-local-introduce
    (null-voided
     #'begin0
     (resolve-with null null (map syntax-local-introduce (attribute tpl)))))])

(define-syntax-parser if-template
  [(_ test pass-tpl fail-tpl)
   (syntax-local-introduce
    (null-voided
     #'begin
     (resolve-if (syntax-local-introduce (attribute test))
                 (syntax-local-introduce (attribute pass-tpl))
                 (syntax-local-introduce (attribute fail-tpl)))))])

(begin-for-syntax
  (define-syntax-class not-else (pattern (~not (~literal else)))))

(define-syntax-parser cond-template
  [(_ [test:not-else then-tpl ...] ...
      (~optional [(~literal else) else-tpl ...]))
   (syntax-local-introduce
    (null-voided
     #'begin
     (resolve-cond (map syntax-local-introduce (attribute test))
                   (map (curry map syntax-local-introduce) (attribute then-tpl))
                   (and (attribute else-tpl)
                        (map syntax-local-introduce (attribute else-tpl))))))])

(define-syntax-parser when-template
  [(_ test-expr tpl ...)
   (syntax-local-introduce
    (null-voided
     #'begin
     (resolve-when (syntax-local-introduce (attribute test-expr))
                   (map syntax-local-introduce (attribute tpl)))))])

(define-syntax-parser unless-template
  [(_ test-expr tpl ...)
   (syntax-local-introduce
    (null-voided
     #'begin
     (resolve-unless (syntax-local-introduce (attribute test-expr))
                     (map syntax-local-introduce (attribute tpl)))))])

(define-syntax-parser for/template
  [(_ ([var:id seq] ...) tpl ...)
   (syntax-local-introduce
    (null-voided
     #'begin
     (resolve-comprehension #'for/list
                            (map syntax-local-introduce (attribute var))
                            (map syntax-local-introduce (attribute seq))
                            (map syntax-local-introduce (attribute tpl)))))])

(define-syntax-parser for*/template
  [(_ ([var:id seq] ...) tpl ...)
   (syntax-local-introduce
    (null-voided
     #'begin
     (resolve-comprehension #'for*/list
                            (map syntax-local-introduce (attribute var))
                            (map syntax-local-introduce (attribute seq))
                            (map syntax-local-introduce (attribute tpl)))))])

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
  (parameterize ([current-resolvers (cdr (current-resolvers))])
    (resyntax stx (syntax-local-eval (resolve-one stx)))))

(define-for-syntax (resolve-untemplate-splicing stx)
  (define results
    (parameterize ([current-resolvers (cdr (current-resolvers))])
      (for/list ([result (in-list (map syntax-local-eval (resolve stx)))])
        (if (list? result) result (syntax-e result)))))
  (map (curry resyntax stx) (flatten results)))

(define-for-syntax (resolve-app stx)
  (resyntax stx (resolve-many (syntax-e stx))))

(define-for-syntax (resolve-pair stx)
  (define stxs (syntax-e stx))
  (list (resyntax stx (cons (resolve-one (car stxs)) (resolve-one (cdr stxs))))))

(define-for-syntax (resolve-with vars args tpls)
  (parameterize ([current-vars (append vars (current-vars))]
                 [current-args (append args (current-args))]
                 [current-resolvers (cons resolve-template (current-resolvers))]
                 [keep-template-scopes? #t]
                 [resolve-inside-syntax? #t]
                 [resolve-outside-syntax? #t])
    (resolve-many tpls)))

(define-for-syntax (resolve-semiwith vars args tpls)
  (parameterize ([current-vars (append vars (current-vars))]
                 [current-args (append args (current-args))]
                 [current-resolvers (cons resolve-template (current-resolvers))]
                 [keep-template-scopes? #f]
                 [resolve-inside-syntax? #t]
                 [resolve-outside-syntax? #t])
    (resolve-many tpls)))

(define-for-syntax (resolve-quote vars args tpls)
  (parameterize ([current-vars (append vars (current-vars))]
                 [current-args (append args (current-args))]
                 [current-resolvers (cons resolve-quote-template (current-resolvers))]
                 [keep-template-scopes? #f]
                 [resolve-inside-syntax? #f]
                 [resolve-outside-syntax? #f])
    (resolve-many tpls)))

(define-for-syntax (resolve-semiquote vars args tpls)
  (parameterize ([current-vars (append vars (current-vars))]
                 [current-args (append args (current-args))]
                 [current-resolvers (cons resolve-quote-template (current-resolvers))]
                 [keep-template-scopes? #t]
                 [resolve-inside-syntax? #f]
                 [resolve-outside-syntax? #f])
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

(define-for-syntax (resolve-comprehension for/? vars seqs tpls)
  (define-values (ctx args-list)
    (syntax-local-eval
     (with-syntax ([(var ...) vars]
                   [(seq ...) (parameterize ([keep-template-scopes? #t])
                                (resolve-many seqs))])
       #`(values #'ctx (#,for/? ([var seq] ...)
                        (map datum->syntax (list #'var ...) (list var ...)))))))
  (define rescope (curryr (make-syntax-delta-introducer ctx #'here) 'remove))
  (define tpls*
    (parameterize ([current-resolvers (cons resolve-quote-template (current-resolvers))]
                   [resolve-inside-syntax? #f]
                   [resolve-outside-syntax? #f])
      (resolve-many tpls)))
  (flatten
   (for/list ([args (in-list args-list)])
     (parameterize ([current-vars (append vars (current-vars))]
                    [current-args (append (map rescope args) (current-args))]
                    [current-comps (append vars (current-comps))]
                    [keep-template-scopes? #t])
       (resolve-many tpls*)))))

(define-for-syntax (null-voided head stxs)
  (cond [(null? stxs) #'(void)]
        [(null? (cdr stxs)) (car stxs)]
        [else #`(#,head #,@stxs)]))

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
    (untemplate untemplate-splicing with-template semiwith-template quote-template
                semiquote-template begin-template begin0-template if-template
                cond-template when-template unless-template for/template for*/template
                let-template))

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
    [(semiwith-template ([var:id arg] ...) tpl ...)
     (resolve-semiwith (attribute var) (attribute arg) (attribute tpl))]
    [(quote-template ([var:id arg] ...) tpl ...)
     (resolve-quote (attribute var) (attribute arg) (attribute tpl))]
    [(semiquote-template ([var:id arg] ...) tpl ...)
     (resolve-semiquote (attribute var) (attribute arg) (attribute tpl))]
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
    [_ (resolve-quote-template this-syntax)]))

(define-for-syntax resolve-quasisyntax
  (syntax-parser
    #:literals (syntax quasisyntax unsyntax unsyntax-splicing)
    #:literal-sets (template-forms)
    [     (syntax tpl) (list      (resolve-syntax-object (attribute tpl)))]
    [(quasisyntax tpl) (list (resolve-quasisyntax-object (attribute tpl)))]
    [   (unsyntax tpl) (list    (resolve-unsyntax-object (attribute tpl)))]
    [  (unsyntax-splicing tpl) (list (resolve-unsyntax-splicing-object (attribute tpl)))]
    [_ (resolve-syntax this-syntax)]))

(define-for-syntax resolve-syntax
  (syntax-parser
    #:literal-sets (template-forms)
    [(untemplate          tpl)    (maybe (list (resolve-untemplate (attribute tpl))))]
    [(untemplate-splicing tpl) (maybe (resolve-untemplate-splicing (attribute tpl)))]
    [(with-template ([var:id arg] ...) tpl ...)
     (maybe (resolve-with (attribute var) (attribute arg) (attribute tpl)))]
    [(semiwith-template ([var:id arg] ...) tpl ...)
     (maybe (resolve-semiwith (attribute var) (attribute arg) (attribute tpl)))]
    [(quote-template ([var:id arg] ...) tpl ...)
     (maybe (resolve-quote (attribute var) (attribute arg) (attribute tpl)))]
    [(semiquote-template ([var:id arg] ...) tpl ...)
     (maybe (resolve-semiquote (attribute var) (attribute arg) (attribute tpl)))]
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
    [_ (resolve-quote-template this-syntax)]))

(define-for-syntax resolve-quote-template
  (syntax-parser
    #:literal-sets (template-forms)
    [(_ ...)   (list (resolve-app     this-syntax))]
    [(_ . _)   (list (resolve-pair    this-syntax))]
    [:vector-t (list (resolve-vector  this-syntax))]
    [:box-t    (list (resolve-box     this-syntax))]
    [:hash-t   (list (resolve-hash    this-syntax))]
    [:prefab-t (list (resolve-prefab  this-syntax))]
    [:literal  (list (resolve-literal this-syntax))]
    [_ (list this-syntax)]))

(define-for-syntax current-vars (make-parameter null))
(define-for-syntax current-args (make-parameter null))
(define-for-syntax current-comps (make-parameter null))
(define-for-syntax current-resolvers (make-parameter (list resolve-template)))
(define-for-syntax keep-template-scopes? (make-parameter #t))
(define-for-syntax resolve-inside-syntax? (make-parameter #t))
(define-for-syntax resolve-outside-syntax? (make-parameter #t))

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
      (if (or (keep-template-scopes?)
              (is-comp? (has-template-vars? str)))
          (string->syntax stx (resolve-vars str))
          (string->syntax (var-arg str) (resolve-vars str)))
      stx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Template Variables

(define-for-syntax (has-template-vars? str)
  (for/or ([var (in-list (current-vars))])
    (and (has-var? str var) var)))

(define-for-syntax (has-var? str var)
  (string-contains? str (syntax->string var)))

(define-for-syntax (is-comp? var)
  (and var (member (syntax->string var)
                   (map syntax->string (current-comps))
                   string=?)))

(define-for-syntax (var-arg str)
  (for/or ([var (in-list (current-vars))]
           [arg (in-list (current-args))])
    (and (has-var? str var) (syntax-local-introduce arg))))

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
;;; Module Templates

(define-simple-macro (template-module-begin (var:id ...) tpl ...)
  #:with template-arity #`#,(length (attribute var))
  #:with ((spec ...) (tpl* ...))
  (call-with-values
   (λ ()
     (for/fold ([specs null]
                [tpls null])
               ([stx (in-list (attribute tpl))])
       (syntax-parse stx
         [((~literal require) spec ...) (values (append specs (attribute spec)) tpls)]
         [_ (values specs (append tpls (list stx)))])))
   list)

  (#%module-begin
   (require template (for-syntax racket/base) spec ...)
   (provide the-template)

   (define-for-syntax (rescope ctx)
     (compose
      (curryr (make-syntax-delta-introducer ctx #f) 'add)
      (curryr (make-syntax-delta-introducer #'template-module-begin #f) 'remove)))

   (define-syntax (the-template stx)
     (syntax-case stx ()
       [(_ arg (... ...))
        (= (length (syntax->list #'(arg (... ...)))) template-arity)
        (with-syntax ([(var* (... ...)) ((rescope stx) #'(var ...))])
          #'(semiwith-template ([var* arg] (... ...)) tpl* ...))]))))

(define-simple-macro (load-template name:id mod-path)
  #:with the-template (datum->syntax this-syntax 'the-template)
  (local-require (rename-in mod-path [the-template name])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #lang template

(module reader syntax/module-reader template/lang)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Developer Tools

(define-simple-macro (debug-template tpl)
  #:with tpl* (local-expand (attribute tpl) 'top-level #f)
  (begin (pretty-display 'tpl*) tpl*))

(define-simple-macro (debug-template/scopes tpl)
  #:with tpl* (syntax-disarm (local-expand (attribute tpl) 'top-level (list #'syntax)) #f)
  (begin (displayln (+scopes #'tpl*)) (print-full-scopes #f) tpl*))

(define-simple-macro (reset-debug-scopes)
  (parameterize ([current-output-port (open-output-string)])
    (print-full-scopes)))
