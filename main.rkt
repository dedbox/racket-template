#lang racket/base

(require racket/splicing
         syntax/parse/define
         (for-syntax racket/base
                     racket/format
                     racket/function
                     racket/list
                     racket/sequence
                     racket/string
                     racket/syntax
                     syntax/parse/define
                     syntax/strip-context)
         (for-meta 2 racket/base
                   syntax/strip-context))

(provide (all-defined-out)
         (for-syntax template templates in-template-ids))

(define-syntax (untemplate stx)
  (raise-syntax-error #f "illegal outside of template forms" stx))

(define-syntax (untemplate-splicing stx)
  (raise-syntax-error #f "illegal outside of template forms" stx))

(define-for-syntax ((expand-and-recontext ctx) stx)
  (replace-context ctx (local-expand stx 'top-level (list #'quote))))

(begin-for-syntax
  (define-simple-macro (templates [(var:id ...) form ...] ...)
    (λ (stx)
      (syntax-parse stx
        [(_ val (... ...))
         #:when (= (length (attribute val))
                   (length (syntax->list #'(var ...))))
         #:with (var* (... ...)) (map syntax-local-introduce (syntax->list #'(var ...)))
         #:with (form* (... ...)) (map syntax-local-introduce (syntax->list #'(form ...)))
         #'(begin-template ([var* val] (... ...)) form* (... ...))]
        ...)))

  (define-simple-macro (template (var:id ...) form ...)
    (templates [(var ...) form ...]))

  (define current-vars (make-parameter #f))
  (define current-vals (make-parameter #f))

  (define (syntax->string stx)
    (syntax-parse stx
      [:id (~a (syntax->datum stx))]
      [(a ...) (~a "(" (string-join (map syntax->string (attribute a))) ")")]
      [_ (~v (syntax->datum stx))]))

  (define (string->syntax ctx str)
    (datum->syntax ctx (read (open-input-string str)) ctx ctx))

  (define (resolve-template stx)
    (syntax-parse stx
      [:id (resolve-id stx)]
      [:string (resolve-string stx)]
      [((~literal quasisyntax) expr)
       #:with expr* (resolve-quasi-template (attribute expr))
       (define ctx (attribute expr))
       (datum->syntax ctx (syntax-e #'#`expr*) ctx ctx)]
      [((~or (~literal unsyntax)
             (~literal untemplate)) expr)
       (template-local-eval (attribute expr) (attribute expr))]
      [(a ...)
       #:with ((a* ...) ...)
       (for/list ([a-stx (in-list (attribute a))])
         (syntax-parse (resolve-template a-stx)
           [((~or (~literal unsyntax-splicing)
                  (~literal untemplate-splicing)) expr)
            (syntax-e (template-local-eval (attribute expr) (attribute expr)))]
           [((~literal for/template) ([var:id seq] ...) expr ...)
            #:with ((_ expr* ...) ...) (map (expand-and-recontext stx)
                                            (iterate #'for/list
                                                     (attribute var)
                                                     (attribute seq)
                                                     (attribute expr)))
            (syntax->list #'((begin expr* ...) ...))]
           [((~literal for*/template) ([var:id seq] ...) expr ...)
            #:with ((_ expr* ...) ...) (map (expand-and-recontext stx)
                                            (iterate #'for*/list
                                                     (attribute var)
                                                     (attribute seq)
                                                     (attribute expr)))
            (syntax->list #'((begin expr* ...) ...))]
           [a** (list (attribute a**))]))
       (datum->syntax stx (flatten (attribute a*)) stx stx)]
      [_ stx]))

  (define (resolve-quasi-template stx)
    (syntax-parse stx
      [:id (resolve-id stx)]
      [:string (resolve-string stx)]
      [((~literal unsyntax) expr)
       #:with expr* (resolve-template (attribute expr))
       (define ctx (attribute expr))
       (datum->syntax ctx (syntax-e #'#,expr*) ctx ctx)]
      [((~literal untemplate) expr)
       (template-local-eval (attribute expr) (attribute expr))]
      [(a ...)
       #:with ((a* ...) ...)
       (for/list ([a-stx (in-list (attribute a))])
         (syntax-parse (resolve-template a-stx)
           [((~literal unsyntax-splicing) expr)
            #:with expr* (resolve-template (attribute expr))
            #'#,@expr*]
           [((~literal untemplate-splicing) expr)
            (syntax-e (template-local-eval (attribute expr) (attribute expr)))]
           [a** (list (attribute a**))]))
       (datum->syntax stx (flatten (attribute a*)) stx stx)]
      [_ stx]))

  (define (template-local-eval ctx stx)
    (datum->syntax ctx (syntax-local-eval (resolve-template stx)) ctx ctx))

  (define (resolve-id stx)
    (define str (syntax->string stx))
    (if (has-template-vars? str) (string->syntax stx (resolve-vars str)) stx))

  (define (resolve-string stx)
    (define str (syntax-e stx))
    (if (has-template-vars? str)
        (datum->syntax stx (resolve-vars str) stx stx)
        stx))

  (define (has-template-vars? str)
    (for/or ([x (in-list (current-vars))]) (string-contains? str x)))

  (define (resolve-vars str)
    (for/fold ([str str])
              ([x (in-list (current-vars))]
               [a (in-list (current-vals))])
      (string-replace str x a))))

(define-syntax-parser begin-template
  [(_ ([var:id val] ...) form ...)
   #:with (form* ...) (parameterize
                          ([current-vars (map syntax->string (attribute var))]
                           [current-vals (map syntax->string (attribute val))])
                        (map resolve-template (attribute form)))
   #`(begin form* ...)])

(define-syntax-parser begin0-template
  [(_ ([var:id val] ...) form ...)
   #:with (form* ...) (parameterize
                          ([current-vars (map syntax->string (attribute var))]
                           [current-vals (map syntax->string (attribute val))])
                        (map resolve-template (attribute form)))
   #`(begin0 form* ...)])

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
  #:with stx this-syntax
  (let-syntax ([go (λ _ ((expand-and-recontext #'stx) (if test #`pass #`fail)))])
    (go)))

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

(begin-for-syntax
  (define (iterate iter-stx vars seqs forms)
    (with-syntax ([iter iter-stx]
                  [(var ...) vars]
                  [(seq ...) seqs]
                  [(form ...) forms])
      (syntax-local-eval
       #'(iter ([var seq] ...)
           #`(begin-template ([var #,var] ...) form ...))))))

(define-simple-macro (for/template ([var:id seq] ...) form ...)
  #:with ((_ form* ...) ...) (map (expand-and-recontext this-syntax)
                                  (iterate #'for/list
                                           (attribute var)
                                           (attribute seq)
                                           (attribute form)))
  (begin (~@ form* ...) ...))

(define-simple-macro (for*/template ([var:id seq] ...) form ...)
  #:with ((_ form* ...) ...) (map (expand-and-recontext this-syntax)
                                  (iterate #'for*/list
                                           (attribute var)
                                           (attribute seq)
                                           (attribute form)))
  (begin (~@ form* ...) ...))

(define-simple-macro (define-template-ids set-id:id member-id:id ...)
  (define-syntax set-id #'(member-id ...)))

(begin-for-syntax
  (define-simple-macro (in-template-ids set-id:id)
    (in-syntax (syntax-local-value #'set-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The #lang

(require racket/base)

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [template-module-begin #%module-begin])
         (for-syntax (all-from-out racket/base)))

(module reader syntax/module-reader template)

(define-simple-macro (template-module-begin (var:id ...) form ...)
  #:with the-template (datum->syntax this-syntax 'the-template this-syntax)
  (#%module-begin
   (require template (for-syntax racket/base syntax/strip-context))
   (provide the-template)
   (define-syntax (the-template stx)
     (define stx* ((syntax-local-eval #'(template (var ...) form ...)) stx))
     ((expand-and-recontext stx) stx*))))

(define-simple-macro (load-template-module name:id mod-path)
  #:with the-template (datum->syntax this-syntax 'the-template this-syntax)
  (local-require (rename-in mod-path [the-template name])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unit Tests

(module+ test
  (require rackunit
           syntax/macro-testing)

  (test-case "untemplate"
    (define-template (power $b $p)
      (if-template (zero? $p) 1 (* $b (power $b (untemplate (sub1 $p))))))
    (define-template (power* $b $p)
      (if-template (zero? $p) 1 `(* $b ,(power* $b (untemplate (sub1 $p))))))
    (check = (power 2 3) 8)
    (check equal? (power* 2 3) '(* 2 (* 2 (* 2 1))))
    (check-exn exn:fail:syntax? (λ () (convert-syntax-error (untemplate 123)))))

  (test-case "untemplate-splicing"
    (define stx (begin-template () #`(1 (untemplate-splicing '(#,#'2 #,#'3)))))
    (check equal? (syntax->datum stx) '(1 2 3))
    (check-exn exn:fail:syntax? (λ () (convert-syntax-error (untemplate-splicing 123)))))

  (test-case "unsyntax outside quasisyntax"
    (let-syntax ([x #'(+ 2 3)])
      (check = (begin-template () (+ 1 #,(syntax-local-value #'x))) 6)))

  (test-case "unsyntax inside quasisyntax"
    (check equal? (syntax->datum (begin-template () #`(+ 1 #,#'(+ 2 3)))) '(+ 1 (+ 2 3))))

  (test-case "unsyntax-splicing outside quasisyntax"
    (check = (begin-template () (+ 1 #,@(list #'2 #'3))) 6))

  (test-case "quasisyntax"
    (check eq? (syntax->datum (begin-template ([$x a]) #`$x)) 'a)
    (check string=? (syntax->datum (begin-template ([$x a]) #`"$x $x $x")) "a a a")
    (check eq? (syntax->datum (begin-template ([$x a]) #`#,'$x)) 'a)
    (check eq? (syntax->datum (begin-template ([$x a]) #`(untemplate '$x))) 'a)
    (check equal? (syntax->datum (begin-template ([$x a]) #`($x $x $x))) '(a a a))
    (check = (syntax->datum (begin-template ([$x 1]) #`10)) 10))

  (test-case "templates"
    (define-syntax tpl (templates [() 0] [($x) $x0] [($x $y) $x00$y00]))
    (check = (tpl) 0)
    (check = (tpl 1) 10)
    (check = (tpl 1 2) 100200))

  (test-case "template"
    (define-syntax tpl (template ($x $y) '($x$y $y$x $xy $yx)))
    (check equal? (tpl a b) '(ab ba ay bx)))

  (test-case "template define"
    (define-syntax tpl (template ($x) (define $xs '($x $x $x))))
    (check-exn exn:fail:syntax? (λ () (convert-syntax-error (tpl a))))
    (tpl a)
    (check equal? as '(a a a)))

  (test-case "string template"
    (check equal?
           (begin-template ([$x 1] [$y 2] [$z !]) '($x-$y$z "$y-$x$z"))
           '(1-2! "2-1!")))

  (test-case "begin-template form"
    (begin-template ([$x 1] [$y 2]) (define x $x0$y0))
    (check = x 1020))

  (test-case "begin-template expr"
    (check = (begin-template ([$x 1] [$y 3]) 123 $x0$y0) 1030))

  (test-case "begin0-template"
    (check = (begin0-template ([$x 1] [$y 4]) $x0$y0 123) 1040))

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
    (define-template (tpl $b) (if-template $b 1 0))
    (check = (tpl #t) 1)
    (check = (tpl #f) 0))

  (test-case "cond-template"
    (define-template (tpl $a)
      (cond-template [(number? $a) 'N]
                     [(boolean? $a) 'B]
                     [(string? $a) 'S]
                     [else '?]))
    (check eq? (tpl 123) 'N)
    (check eq? (tpl "\"x\"") 'S)
    (check eq? (tpl #f) 'B)
    (check eq? (tpl '()) '?))

  (test-case "when-template"
    (define-template (tpl $b) (when-template $b 1))
    (check-pred void? (tpl #f))
    (check = (tpl #t) 1))

  (test-case "unless-template"
    (define-template (tpl $b) (unless-template $b 0))
    (check-pred void? (tpl #t))
    (check = (tpl #f) 0))

  (test-case "for/template"
    (for/template ([$x (in-syntax #'(A B C))]
                   [$a (in-naturals)])
      (define $x $a0))
    (check = A  0)
    (check = B 10)
    (check = C 20)
    (check equal? (begin-template () (list (for/template ([$m (in-range 3)]
                                                          [$n (in-range 3)])
                                             (+ $n (* $m 3)))))
           '(0 4 8)))

  (test-case "for*/template"
    (for*/template ([$x (in-syntax #'(A B C))]
                    [$y (in-range 3)])
      (define $x$y (add1 $y)))
    (check equal? (list A0 A1 A2 B0 B1 B2 C0 C1 C2) '(1 2 3 1 2 3 1 2 3))
    (check equal? (begin-template () (list (for*/template ([$m (in-range 3)]
                                                           [$n (in-range 3)])
                                             (+ $n (* $m 3)))))
           '(0 1 2 3 4 5 6 7 8)))

  (test-case "identifier sets"
    (define-template-ids ops + - * /)
    (for/template ([$op (in-template-ids ops)]
                   [$want (in-list '(6 -4 6 1/6))])
      (check-true (= ($op 1 2 3) $want)))))
