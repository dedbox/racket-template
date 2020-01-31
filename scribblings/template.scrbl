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

#lang scribble/manual

@title{Template Macros}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./template-includes.rkt}

@require[
  @for-label[
    racket/base
    racket/contract
    racket/function
    racket/sequence
    racket/syntax
    (except-in template #%module-begin)
  ]
]

@example[#:hidden
  @require[
    racket/contract
    racket/function
    (except-in template #%module-begin)
    @for-syntax[
      racket/base
      racket/sequence
    ]
  ]
]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@defmodule[template]

@section{Overview}

@deftech{Template macros} are smilar to @gtech{pattern-based macros}, but with
two important differences:

@itemlist[

  @item{Variable are resolved @emph{within} every internable datum
  @emph{before} macro @rtech{expansion}.}

  @item{Variables are @emph{always} in scope regardless of position, quoting
  depth, or escape status.}

]

@subsection{The `@racketid[$]' Convention}

Throughout this manual, names of @tech{template macro} variables start with a
`@racketid[$]'. The @racketmodname[template] API imposes no such restriction
on variable names.

Beware, because @tech{template macro} variable resolution is finer-grained
than ordinary variable resolution, poorly chosen variable names can lead to
strange compile-time errors.

Examples:
@example[
(eval:error (with-template ([e X]) (define e 123)))
(eval:error (with-template ([e X]) 'e))
]

@; -----------------------------------------------------------------------------

@section{Constructors}

@defform[(template (var-id ...) tpl ...)]{

  Produces a @rtech{syntax transformer} for a macro that accepts an argument
  for each @var[var-id] and substitutes them into the @var[tpl]s.

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

@defform[(templates [(var-id ...) tpl ...] ...)]{

  Produces a @rtech{syntax transformer} procedure. Each @racket[[(var-id ...)
  tpl ...]] clause is analogous to a single @racket[template] procedure;
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
(eval:error (f one two three))
  ]
}

@deftogether[(
@defform[(untemplate expr)]
@defform[(untemplate-splicing expr)]
)]{

  Escapes from an expanding template and replaces itself with the result of
  @racket[expr], an expression at @rtech{phase level} 1 relative to the
  surrounding context.

  Examples:
  @example[
(let-syntax ([x #'(+ 2 3)])
  (begin-template
    (+ 1 (untemplate (syntax-local-value #'x)))))
(begin-template
  '(1 (untemplate-splicing '(2 3))))
  ]

  Results that are not @rtech{syntax objects} acquire the @rtech{lexical
  information}, source-location information, and @rtech{syntax properties} of
  the expressions that produce them.

  Examples:
  @example[
(begin-template #`(untemplate 'here))
(begin-template #`(untemplate (datum->syntax #f 'nowhere)))
  ]

  As a notational convenience, @racket[unsyntax] / @racket[unsyntax-splicing]
  forms occurring inside a template and outside a @racket[quasisyntax] are
  aliased to @racket[untemplate] / @racket[untemplate-splicing].

  Examples:
  @EXAMPLE[
(let-syntax ([x #'(+ 2 3)])
  (begin-template
    (+ 1 #,(syntax-local-value #'x))))
(begin-template
  '(1 #,@'(2 3)))
  ]

  But this notation is disabled in the body of a @racket[syntax] form.

  Example:
  @EXAMPLE[
(begin-template #'#,no-escape)
  ]
}

@; -----------------------------------------------------------------------------

@section{Binding Forms}

@defform[(with-template ([var-id val-expr] ...) tpl ...)]{

  Substitutes occurrences of @var[var-id]s with corresponding @var[val-expr]s
  in the @var[tpl]s comprising its body, then replaces itself with the
  results.

  When a @racket[with-template] form appears at the top level, at module
  level, or in an internal-definition position (before any expression in the
  internal-definition sequence), it is equivalent to splicing the expanded
  @var[tpl]s into the enclosing context.

  Examples:
  @example[
(with-template ([$x a]
                [$y b])
  (define ($x-$y? obj)
    (equal? obj '($x $y))))
(a-b? '(a b))
(a-b? '(c d))
(list (with-template ([$x a]) '$x)
      (with-template ([$x b]) '$x))
  ]

  Non-identifier literals may also be created via @tech{template macro}
  variable resolution.

  Example:
  @example[
(with-template ([$x #f]
                [$y 2])
  (list (not $x) (+ $y0 1)))
  ]
}

@defform[(quote-template ([var-id val-expr] ...) tpl ...)]{

  Like @racket[with-template], except code generated with @tech{template
  macro} variables inherit the @rtech{lexical information}, source-location
  information, and @rtech{syntax properties} of the expression bound to the
  first variable encountered.

  Example:
  @example[
(quote-template ([$where there]) #`(untemplate #'$where))
(with-template ([$where here]) #`(untemplate '$where))
  ]
}

@defform[(define-template (id var-id ...) tpl ...)]{

  Creates a @rtech{transformer} binding of @var[id] to @racket[(template
  (var-id ...) tpl ...)].

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

@defform[(let-template ([(id var-id ...) tpl ...] ...) body-tpl ...)]{

  Creates a @rtech{transformer} binding of each @var[id] with
  @racket[(template (var-id ...) tpl ...)], which is an expression at
  @rtech{phase level} 1 relative to the surrounding context. Each @var[id] is
  bound in the @var[body-tpl]s, and not in other @var[tpl]s.

  Example:
  @example[
(let-template ([(fwd $x $y) '$x$y]
               [(rev $x $y) '$y$x])
  (list (fwd a b) (rev a b)))
  ]
}

@defform[(letrec-template ([(id var-id ...) tpl ...] ...) body-tpl ...)]{

  Like @racket[let-template], except that each @var[var-id] is also bound
  within all remaining @var[tpl]s.

  Example:
  @EXAMPLE[
(letrec-template
    ([(is-even? $n) (if-template (zero? $n) #t (is-odd? #,(sub1 $n)))]
     [(is-odd? $n) (not (is-even? $n))])
  (list (is-even? 10) (is-even? 11)))
  ]
}

@deftogether[(
@defform[(splicing-let-template ([(id var-id ...) tpl ...] ...) body-tpl ...)]
@defform[(splicing-letrec-template ([(id var-id ...) tpl ...] ...) body-tpl ...)]
)]{

  Like @racket[let-template] and @racket[letrec-template], except that in a
  definition context, the @var[body-tpl]s are spliced into the enclosing
  definition context (in the same way as for @racket[with-template]).

  Examples:
  @example[
(splicing-let-template ([(one) 1])
  (define o (one)))
o
(eval:error (one))
  ]

  @EXAMPLE[
(splicing-letrec-template
    ([(is-even? $n) (if-template (zero? $n) #t (is-odd? #,(sub1 $n)))]
     [(is-odd? $n) (not (is-even? $n))])
  (define is-11-even? (is-even? 11))
  (define is-10-even? (is-even? 10)))
(list is-11-even? is-10-even?)
  ]
}

@; -----------------------------------------------------------------------------

@section{Template Combiners}

@defform[(begin-template tpl ...)]{

  Equivalent to @racket[(with-template () tpl ...)].

  Example:
  @example[
(begin-template 1 2 3)
  ]
}

@defform[(begin0-template ([var-id val-expr] ...) tpl ...)]{

  Like @racket[begin-template], except the result of the first @var[tpl] is
  the result of the @racket[begin0-template] form.

  Example:
  @example[
(begin0-template 1 2 3)
  ]
}

@defform[(if-template test-expr then-tpl else-tpl)]{

  Evaluates @var[test-expr], which is an expression at @rtech{phase level} 1
  relative to the surrounding context. If it produces any value other than
  @racket[#f], then @var[then-tpl] takes its place. Otherwise, @var[else-tpl]
  takes its place.

  Examples:
  @example[
(begin-template
  `(if-template (positive? -5) ,(error "doesn't get here") >>))
(begin-template
  `(if-template (positive? 5) << ,(error "doesn't get here")))
(let-syntax ([x 'we-have-no-bananas])
  (with-template ([$+ yes]
                  [$- no])
    (if-template (syntax-local-value #'x) "$+" "$-")))
(let-syntax ([x #f])
  (with-template ([$+ yes]
                  [$- no])
    (if-template (syntax-local-value #'x) "$+" "$-")))
  ]
}

@defform[
  #:literals (else)
  (cond-template [test-expr tpl ...] ... maybe-else-clause)
  #:grammar [(maybe-else-clause (code:line)
                                [else tpl ...])]
]{

  A clause that starts with @racket[else] must be the last clause.

  If no clauses are present, @racket[(void)] takes its place.

  If the first clause does not start with @racket[else] and its
  @var[test-expr], which is an expression at @rtech{phase level} 1 relative to
  the surrounding context, produces @racket[#f], then the result is the same
  as a @racket[cond-template] form with the remaining clauses. Otherwise,
  @racket[(begin tpl ...)] takes its place.

  Examples:
  @example[
(cond-template)
(cond-template [else 5])
(let-syntax ([x #f]
             [y #t])
  (begin-template
    `(cond-template
       [(positive? -5) ,(error "doesn't get here")]
       [(syntax-local-value #'x) ,(error "doesn't get here, either")]
       [(syntax-local-value #'y) here])))
  ]
}

@defform[(when-template test-expr tpl ...)]{

  Evaluates @var[test-expr], which is an expression at @rtech{phase level} 1
  relative to the surrounding context. If the result is not @racket[#f], then
  @racket[(begin tpl ...)] takes its places. otherwise, @racket[(void)] takes
  its place.

  Examples:
  @example[
(displayln (when-template (positive? -5) 'hi))
(let-syntax ([x #t])
  (begin-template
    (list (when-template (syntax-local-value #'x)
            'hey 'there))))
  ]
}

@defform[(unless-template test-expr tpl ...)]{

  Equivalent to @racket[(when-template (not test-expr) tpl ...)].

  Examples:
  @example[
(displayln (unless-template (positive? -5) 'hi))
(let-syntax ([x #t])
  (begin-template
    (list (unless-template (syntax-local-value #'x)
            'hey 'there))))
  ]
}

@defform[(for/template ([var-id seq-expr] ...) tpl ...)]{

  Iteratively evaluates the @var[tpl]s. The @racket[seq-expr]s are evaluated
  left-to-right at phase 1, and each must produce a @rtech{sequence} whose
  elements are syntax objects or primitive values.

  Example:
  @example[
(for/template ([$x (in-syntax #'(A B C))]
               [$n (in-naturals)])
  (define $x (add1 $n)))
(list A B C)
  ]

  When @racket[for/template] is used in an @rtech{expression context} inside a
  template, the resultss are spliced into the enclosing context.

  Example:
  @example[
(begin-template
  (list (for/template ([$n (in-range 10)]) $n)))
  ]

  When used outside any other template, the results are wrapped with
  @racket[begin].

  Example:
  @example[
(list (for/template ([$n (in-range 10)]) $n))
  ]
}

@defform[(for*/template ([var-id seq-expr] ...) tpl ...)]{

  Like @racket[for/template], but with all of its sequence iterations nested.

  Examples:
  @example[
(begin-template
  (list (for*/template ([$m (in-range 3)]
                        [$n (in-range 3)])
          (+ $n (* $m 3)))))
  ]
}

@; -----------------------------------------------------------------------------

@section{Identifier Sequences}

@defform[(define-template-ids id member-id ...)]{

  Defines @var[id] as a list of @rtech{identifiers} for use with
  @racket[in-template-ids].

}

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

@defform[(load-template-module id mod-path)]{

  Binds @var[id] to the @tech{template macro} provided by @var[mod-path].

  Example:
  @example[
(load-template-module tpl template/lang-test)
(tpl a)
as
  ]
}

@defform[(#%module-begin (var-id ...) form ...)]{

  Exports a binding of @var[the-template] to @racket[(template (var-id ...)
  form ...)].

  Example:
  @example[
(module my-template-mod template
  ($x)
  (define $xs '($x $x $x $x)))
(require 'my-template-mod)
(the-template b)
bs
  ]
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[scribble/example]

@close-eval[template-evaluator]
