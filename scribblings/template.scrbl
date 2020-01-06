#lang scribble/manual

@title{Template Macros}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./template-includes.rkt}

@require[
  @for-label[
    racket/base
    racket/sequence
    (except-in template #%module-begin)
  ]
]

@example[#:hidden
  @require[
    (except-in template #%module-begin)
    @for-syntax[
      racket/base
      racket/sequence
    ]
  ]
]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@defmodule[template]

By convention, template variable names are denoted with a leading
`@racketid[$]'. The @racketmodname[template] API imposes no such restriction
on the names of template variables.

@; -----------------------------------------------------------------------------

@section{Construction}

@defform[(template (var-id ...) form ...)]{

  Produces a @rtech{syntax transformer} whose uses accept one argument per
  @var[var-id] and then substitutes them into the @var[form]s.

  Example:
  @example[
    (define-syntax iterate-with
      (template ($for)
        ($for/list ([x (in-range 3)]
                    [y (in-range 3)])
          (+ y (* x 3)))))
    (iterate-with for)
    (iterate-with for*)
  ]
}

@defform[(templates [(var-id ...) form ...] ...)]{

  Produces a @rtech{syntax transformer} procedure. Each @racket[[(var-id ...)
  form ...]] clause is analogous to a single @racket[template] procedure;
  applying the @racket[templates]-generated procedure is the same as applying
  a procedure that corresponds to one of the clauses---the first procedure
  that accepts the given number of arguments. If no corresponding procedure
  accepts the given number of arguments, a syntax error is raised.

  Example:
  @example[
    (define-syntax f
      (templates
        [() 0]
        [($x) '$x]
        [($x $y) '$x-$y]))
    (list (f) (f one) (f one two))
  ]

  @example[
      (eval:error (f one two three))
  ]
}

@deftogether[(
@defform[(untemplate expr)]
@defform[(untemplate-splicing expr)]
)]{

  Escapes from an expanding template and replaces itself with the result of
  the expanded @racket[expr], an expression at @rtech{phase level} 1 relative
  to the surrounding context.

  Examples:
  @example[
    (let-syntax ([x #'(+ 2 3)])
      (begin-template ()
        (+ 1 (untemplate (syntax-local-value #'x)))))
    (begin-template () '(1 (untemplate-splicing '(2 3))))
  ]

  As a notational convenience, @racket[unsyntax] / @racket[unsyntax-splicing]
  forms occurring inside a template and outside a @racket[quasisyntax] are
  aliased to @racket[untemplate] / @racket[untemplate-splicing].

  Example:
  @example[#:escape UNSYNTAX
    (let-syntax ([x #'(+ 2 3)])
      (begin-template ()
        (+ 1 #,(syntax-local-value #'x))))
    (begin-template () '(1 #,@'(2 3)))
  ]
}

@; -----------------------------------------------------------------------------

@section{Binding Forms}

@defform[(define-template (id var-id ...) form ...)]{

  Creates a @rtech{transformer} binding of @var[id] to @racket[(template
  (var-id ...) form ...)].

  Example:
  @example[
    (define-template (iterate-with $for)
      ($for/list ([x (in-range 3)]
                  [y (in-range 3)])
        (+ y (* 3 x))))
    (iterate-with for)
    (iterate-with for*)
  ]
}

@defform[(let-template ([(id var-id ...) form ...] ...) body ...)]{

  Creates a @rtech{transformer} binding of each @var[id] with
  @racket[(template (var-id ...) form ...)], which is an expression at
  @rtech{phase level} 1 relative to the surrounding context. Each @var[id] is
  bound in the @var[body]s, and not in other @var[form]s.

  Example:
  @example[
    (let-template ([(fwd $x $y) '$x$y]
                   [(rev $x $y) '$y$x])
      (list (fwd a b) (rev a b)))
  ]
}

@defform[(letrec-template ([(id var-id ...) form ...] ...) body ...)]{

  Like @racket[let-template], except that each @var[var-id] is also bound
  within all remaining @var[form]s.

  Example:
  @example[#:escape UNSYNTAX
    (letrec-template
        ([(is-even? $n) (if-template (zero? $n) #t (is-odd? #,(sub1 $n)))]
         [(is-odd? $n) (not (is-even? $n))])
      (list (is-even? 10) (is-even? 11)))
  ]
}

@deftogether[(
@defform[(splicing-let-template ([(id var-id ...) form ...] ...) body ...)]
@defform[(splicing-letrec-template ([(id var-id ...) form ...] ...) body ...)]
)]{

  Like @racket[let-template] and @racket[letrec-template], except that in a
  definition context, the @var[body]s are spliced into the enclosing
  definition context (in the same way as for @racket[begin-template]).

  Examples:
  @example[
    (splicing-let-template ([(one) 1])
      (define o (one)))
    o
    (eval:error (one))
  ]

  @example[#:escape UNSYNTAX
    (splicing-letrec-template
        ([(is-even? $n) (if-template (zero? $n) #t (is-odd? #,(sub1 $n)))]
         [(is-odd? $n) (not (is-even? $n))])
      (define is-11-even? (is-even? 11))
      (define is-10-even? (is-even? 10)))
    (list is-11-even? is-10-even?)
  ]
}

@; -----------------------------------------------------------------------------

@section{Sequence, Selection, Iteration}

@defform*[((begin-template ([var-id val-id] ...) form ...)
           (begin-template ([var-id val-id] ...) expr ...))]{

  Substitutes occurrences of @var[var-id]s with corresponding @var[val-id]s
  inside the @rtech{identifiers} in the @var[form]s or @var[expr]s comprising
  its body.

  The first form applies when @racket[begin-template] appears at the top
  level, at module level, or in an internal-definition position (before any
  expression in the internal-definition sequence). In that case, the
  @racket[begin-template] form is equivalent to splicing the expanded
  @var[form]s into the enclosing context.

  The second form applies for @racket[begin-template] in an expression
  position. In that case, the expanded @var[expr]s take its place.

  Examples:
  @example[
    (begin-template ([$x a] [$y b])
      (define ($x-$y? obj)
        (equal? obj '($x $y))))
    (a-b? '(a b))
    (a-b? '(c d))
  ]

  @example[
    (list (begin-template ([$x a]) '$x)
          (begin-template ([$x b]) '$x))
  ]

  Non-identifier literals may also be created via substitution.

  Example:
  @example[
    (begin-template ([$x #f] [$y 2])
      (list (not $x) (+ $y0 1)))
  ]
}

@defform[(begin0-template ([var-id val-id] ...) expr ...)]{

  Like @racket[begin-template], except the results of the first @var[expr] are
  the results of the @racket[begin0-template] form.

  Example:
  @example[
    (begin0-template ([$x a] [$y b])
      (values '$x '$y)
      (displayln 'hi))
  ]
}

@defform[(if-template test-expr then-expr else-expr)]{

  Evaluates @var[test-expr], which is an expression at @rtech{phase level} 1
  relative to the surrounding context. If it produces any value other than
  @racket[#f], then @var[then-expr] takes its place. Otherwise,
  @var[else-expr] takes its place.

  Examples:
  @example[
    (begin-template ()
      (if-template (positive? -5) (error "doesn't get here") 2))
    (begin-template ()
      (if-template (positive? 5) 1 (error "doesn't get here")))
    (let-syntax ([x 'we-have-no-bananas])
      (begin-template ()
        (if-template (syntax-local-value #'x) "yes" "no")))
    (let-syntax ([x #f])
      (begin-template ()
        (if-template (syntax-local-value #'x) "yes" "no")))
  ]
}

@defform[
  #:literals (else)
  (cond-template [test-expr then-body ...] ... maybe-else-clause)
  #:grammar [(maybe-else-clause (code:line)
                                [else then-body ...])]
]{

  A clause that starts with @racket[else] must be the last clause.

  If no clauses are present, @racket[(void)] takes its place.

  If the first clause does not start with @racket[else] and its
  @var[test-expr], which is an expression at @rtech{phase level} 1 relative to
  the surrounding context, produces @racket[#f], then the result is the same
  as a @racket[cond-template] form with the remaining clauses. Otherwise,
  @racket[(begin then-body ...)] takes its place.

  Examples:
  @example[
    (begin-template () (cond-template))
    (begin-template () (cond-template [else 5]))
    (begin-template ()
      (let-syntax ([x #f] [y #t])
        (cond-template
          [(positive? -5) (error "doesn't get here")]
          [(syntax-local-value #'x) (error "doesn't get here, either")]
          [(syntax-local-value #'y) 'here])))
  ]
}

@defform[(when-template test-expr body ...)]{

  Evaluates @var[test-expr], which is an expression at @rtech{phase level} 1
  relative to the surrounding context. If the result is not @racket[#f], then
  @racket[(begin body ...)] takes its places. otherwise, @racket[(void)] takes
  its place.

  Examples:
  @example[
    (let-syntax ([x #t])
      (begin-template ()
        (when-template (positive? -5) (displayln 'hi))
        (when-template (syntax-local-value #'x)
          (display 'hi)
          (display 'there))))
  ]
}

@defform[(unless-template test-expr body ...)]{

  Equivalent to @racket[(when-template (not test-expr) body ...)].

  Examples:
  @example[
    (let-syntax ([x #f])
      (begin-template ()
        (unless-template (positive? 5) (displayln 'hi))
        (unless-template (syntax-local-value #'x)
          (display 'hi)
          (display 'there))))
  ]
}

@defform[(for/template ([var-id seq-expr] ...) body ...)]{

  Iteratively evaluates a template macro. The @racket[seq-expr]s are evaluated
  left-to-right at phase 1, and each must produce a @rtech{sequence} whose
  elements are syntax objects or primitive values.

  Example:
  @example[
    (for/template ([$x (in-syntax #'(A B C))]
                   [$n (in-naturals)])
      (define $x (add1 $n)))
    (list A B C)
  ]
}

@defform[(for*/template ([var-id seq-expr] ...) body ...)]{

  Like @racket[for/template], but with all of its sequence iterations nested.

  Example:
  @example[
    (for*/template ([$x (in-syntax #'(A B C))]
                    [$n (in-range 3)])
      (define $x$n (add1 $n)))
    (list A0 A1 A2 B0 B1 B2 C0 C1 C2)
  ]
}

@; -----------------------------------------------------------------------------

@section{Identifier Sequences}

@defform[(define-template-ids id member-id ...)]{

  Defines @var[id] as a list of @rtech{identifiers} for use with
  @racket[in-template-ids].

}

@; @defproc[(in-template-ids [id identifier?]) sequence?]{

@defform[(in-template-ids id)]{

  Produces a sequence whose elements are the successive identifiers bound to
  @var[id] by @racket[define-template-ids].

  Example:
  @example[
    (define-template-ids operators + - * /)
    (for/template ([$op (in-template-ids operators)])
      (displayln ($op 2 3)))
  ]
}

@; -----------------------------------------------------------------------------

@section{Modules}

In @racketcommentfont{template/lang-test.rkt}:

@codeblock|{
  #lang template ($x)

  (define $xs '($x $x $x))
}|

@defform[(#%module-begin (var-id ...) form ...)]{

  Exports a binding of @var[the-template] to @racket[(template (var-id ...)
  form ...)].

  Example:
  @example[
    (module my-template-mod template
      ($x)
      (define $xs '($x $x $x $x)))
    (require 'my-template-mod)
    (the-template a)
    as
  ]
}

@defform[(load-template-module id mod-path)]{

  Binds @var[id] to the template macro provided by @var[mod-path].

  Example:
  @example[
    (load-template-module tpl template/lang-test)
    (tpl b)
    bs
  ]
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[scribble/example]

@close-eval[template-evaluator]
