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
  @for-syntax[
    racket/base
    racket/file
    racket/function
    racket/runtime-path
    racket/syntax
  ]
  @for-label[
    debug-scopes
    racket/base
    racket/contract
    racket/function
    racket/sequence
    racket/syntax
    template
  ]
]

@example[#:hidden
  @require[
    racket/contract
    racket/function
    template
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
four important differences:

@itemlist[

  @item{Template variables are resolved @emph{within} every internable datum
  and can be combined to synthesize non-identifier literals.

  @example[
(with-template ([$x 3] [$y 2]) (add1 $x$y0))
  ]}

  @item{Templates can escape to the expanding environment, even when they
  appear @emph{outside} of a @rtech{syntax transformer}.

  @example[
(begin-template '(#,@(list #'hello #'world)))
  ]}

  @item{Most templates become splicing forms when used inside other templates.

  @example[
(begin-template (list (for/template ([$k 10]) (add1 $k))))
  ]}

  @item{Template variables are @emph{always} in scope, regardless of
  position, quoting depth, or escape level.

  @example[
(begin-template '((for/template ([$a 3]) #'"$a")))
  ]
  }

]

The @racketmodname[template] API splits ordinary macro @rtech{expansion} into
two distinct stages: @deftech{template expansion} time and @deftech{macro
expansion} time. Always, template variables are resolved and sub-template
forms are expanded before ordinary macros are expanded.

@subsection{The `@racketid[$]' Convention}

Throughout this manual, the names of template variables start with a
`@racketid[$]'. Although the @racketmodname[template] API imposes no such
restriction on variable names, beware:

@tech{Template macro} variable resolution is finer-grained than ordinary
variable resolution. Poorly chosen variable names can lead to bizarre syntax
errors.

Examples:
@example[
(eval:error (with-template ([e X]) (define e 123)))
(eval:error (with-template ([e X]) 'e))
]

@; -----------------------------------------------------------------------------

@section{Primitives}

@defform[(with-template ([var-id val-expr] ...) tpl ...)]{

  Expands the sub-templates of the @var[tpl]s and substitutes @var[var-id]s
  with @var[val-expr]s while retaining the @rtech{lexical information},
  source-location information, and @rtech{syntax properties} of the
  originating template source.

  Example:
  @example[
(with-template ([$x a]
                [$y b])
  (define $xs '($x $x $x))
  (displayln `($xs = ,@$xs)))
  ]

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
(list
 (with-template ([$x a]) '$x)
 (with-template ([$x b]) '$x))
  ]
}

@defform[(semiquote-template ([var-id val-expr] ...) tpl ...)]{

  Like @racket[with-template], except sub-templates wrapped in @racket[syntax]
  or @racket[quasisyntax] are not expanded, and code fragments generated from
  template variables inherit the @rtech{lexical information}, source-location
  information, and @rtech{syntax properties} of the @var[val-expr] bound to
  the first @var[var-id] used.

  Example:
  @example[
(semiquote-template ([$where 'here]) #'$where)
  ]

  @example[
(with-template ([$where 'not--here]) #'$where)
  ]

  When a @racket[semiquote-template] form appears at the top level, at module
  level, or in an internal-definition position (before any expression in the
  internal-definition sequence), it is equivalent to splicing the expanded
  @var[tpl]s into the enclosing context.
}

@defform[(quote-template ([var-id val-expr] ...) tpl ...)]{

  Like @racket[semiquote-template], except sub-templates are not expanded.

  Example:
  @example[
(quote-template ([$n 5])
  '((for/template ([$k (in-range $n)]) $k)))
  ]

  @example[
(semiquote-template ([$n 5])
  '((for/template ([$k (in-range $n)]) $k)))
  ]
}

@; -----------------------------------------------------------------------------

@section{Constructors}

@deftogether[(
@defform[(template (var-id ...) tpl ...)]
@defform[(quoted-template (var-id ...) tpl ...)]
@defform[(semiquoted-template (var-id ...) tpl ...)]
)]{

  Produces a @tech{template macro} procedure; a @rtech{syntax transformer} for
  a macro that accepts an argument for each @var[var-id] and substitutes them
  into the @var[tpl]s.

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

@deftogether[(
@defform[(templates [(var-id ...) tpl ...] ...)]
@defform[(quoted-templates [(var-id ...) tpl ...] ...)]
@defform[(semiquoted-templates [(var-id ...) tpl ...] ...)]
)]{

  Produces a @tech{template macro} procedure. Each @racket[[(var-id ...) tpl
  ...]] clause is analogous to a single @racket[template],
  @racket[quoted-template], or @racket[semiquoted-template] procedure;
  applying the @racket[templates]-generated procedure is the same as applying
  a procedure that corresponds to one of the clauses---the first procedure
  that accepts the given number of arguments. If no corresponding procedure
  accepts the given number of arguments, a syntax error is raised.

  Example:
  @example[
(define-syntax f
  (templates [() 0]
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

  If @var[expr] does not evaluate to a @rtech{syntax object}, the result is
  wrapped in the @rtech{lexical information}, source-location information, and
  @rtech{syntax properties} of @var[expr].

  Examples:
  @example[
(begin-template #'(untemplate 'here))
(begin-template #'(untemplate (datum->syntax #f 'nowhere)))
  ]
}

@; -----------------------------------------------------------------------------

@section{Combiners}

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

  If no clauses are present, @racket[(void)] takes the place of the
  @racket[cond-template] form.

  If the first clause does not start with @racket[else] and its
  @var[test-expr], which is an expression at @rtech{phase level} 1 relative to
  the surrounding context, produces @racket[#f], then the result is the same
  as a @racket[cond-template] form with the remaining clauses. Otherwise, the
  @var[tpl]s take the place of the @racket[cond-template] form.

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
  the @var[tpl]s takes the place of the @racket[when-template] form.
  Otherwise, @racket[(void)] takes its place.

  Examples:
  @example[
(displayln (when-template #f 'hi))
(begin-template
  (list (when-template #t 'hey 'there)))
  ]
}

@defform[(unless-template test-expr tpl ...)]{

  Equivalent to @racket[(when-template (not test-expr) tpl ...)].

  Examples:
  @example[
(displayln (unless-template #f 'hi))
(begin-template
  (list (unless-template #t 'hey 'there)))
  ]
}

@defform[(for/template ([var-id seq-expr] ...) tpl ...)]{

  Iterates like @racket[for/list], but results are accumulated into a
  @racket[begin-template] instead of a list.

  Example:
  @example[
(for/template ([$x (in-syntax #'(A B C))]
               [$n (in-naturals)])
  (define $x (add1 $n)))
(list A B C)
  ]

  When @racket[for/template] is used in an @rtech{expression context} inside a
  template, the results are spliced into the enclosing context.

  Example:
  @example[
(begin-template
  (list (for/template ([$n (in-range 10)]) $n)))
  ]

  When used outside any other template, the results are wrapped with
  @racket[begin].

  Example:
  @example[
(list (for/template ([$n 10]) (add1 $n)))
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

@section{Binding Forms}

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

@defform[(define-templates [(id var-id ...) tpl ...] ...)]{

  Like @racket[define-template], but creates a @rtech{transformer} binding for
  each @var[id].

  Example:
  @example[
(define-templates
  [(print-one $obj) (displayln "$obj")]
  [(print-many $objs)
   (for/template ([$obj (in-syntax #'$objs)])
     (print-one $obj))])
(print-many (one two three))
  ]
}

@defform[(let-template ([(id var-id ...) tpl ...] ...) body-tpl ...)]{

  Creates a @rtech{transformer} binding of each @var[id] with
  @racket[(template (var-id ...) tpl ...)]. Each @var[id] is bound in the
  @var[body-tpl]s, and not in other @var[tpl]s.

  Example:
  @example[
(let-template ([(fwd $x $y) '$x$y]
               [(rev $x $y) '$y$x])
  (list (fwd a b) (rev a b)))
  ]
}

@defform[(letrec-template ([(id var-id ...) tpl ...] ...) body-tpl ...)]{

  Like @racket[let-template], except that each @var[var-id] is also bound
  within all @var[tpl]s.

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

@section{Module Templates}

In @racketcommentfont{template/tests/lang-template.rkt}:

@(begin-for-syntax (define-runtime-path lang-tpl-path "lang-template.rkt"))
@(let-syntax ([go (λ _
  (syntax-local-eval
   #`#'(codeblock
       #,@(let loop ([lines (file->lines lang-tpl-path)])
            (if (string=? (car lines) "")
                (map (curryr string-append "\n") (cdr lines))
                (loop (cdr lines)))))))])
   (go))

@defform[(load-template id mod-path)]{

  Binds @var[id] to the @tech{template macro} provided by @var[mod-path].

  Example:
  @example[
(load-template tpl template/scribblings/lang-template)
(tpl a 4)
as
a4
  ]
}

@defform[(template-module-begin (var-id ...) tpl ...)]{

  Exports a binding of @var[the-template] to @racket[(template (var-id ...)
  tpl ...)].

  Example:
  @example[
(module my-template-mod template/lang
  ($x)
  (define $xs '($x $x $x $x)))
(require 'my-template-mod)
(the-template b)
bs
  ]
}

@; -----------------------------------------------------------------------------

@section{Developer Tools}

@defform[(debug-template tpl)]{

  Displays the expanded form of @var[tpl], then evaluates the result.

  Examples:
  @example[
(debug-template (for/template ([$n 10]) $n))
  ]

  @example[
(debug-template (begin-template (list (for/template ([$n 10]) $n))))
  ]
}

@defform[(debug-template/scopes tpl)]{

  Displays the expanded form of @var[tpl], annotated with superscripts
  indicating the scopes present on them, then evaluates the result.

  Example:
  @example[
(debug-template/scopes
  (for/template ([$x (in-syntax #'(A B C D E))]
                 [$n (in-range 1 6)])
    #'$x$n))
  ]
}

@defform[(reset-debug-scopes)]{

  Resets the internal counter used by @racket[debug-template/scopes] to print
  a summary table.

}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[scribble/example]

@close-eval[template-evaluator]
