#lang scribble/manual

@title{Template Macros}
@author{@author+email["Eric Griffis" "dedbox@gmail.com"]}

@require{./template-includes.rkt}

@require[
  @for-label[
    racket/base
    racket/sequence
    template
  ]
]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{API Reference}

As a convention, and for the reader's convenience, all template variable names
are denoted with a leading `@racketid[$]'. The @racketmodname[template] API
imposes no such restriction on the names of template variables.

@defmodule[template]

@defform[(begin-template ([var-id val-id] ...) form ...)]{

  Substitutes occurrences of @var[var-id]s with corresponding @var[val-id]s
  inside the @rtech{identifiers} in the @var[form]s comprising its body.

  Example:
  @example[
    (begin-template ([$x a] [$y b])
      (define ($x-$y? obj)
        (equal? obj '($x $y))))
    (a-b? '(a b))
    (a-b? '(c d))
  ]

  Non-identifier literals may also be created by substitution.

  Example:
  @example[
    (begin-template ([$x #f] [$y 2])
      (list (not $x) (+ $y0 1)))
  ]
}

@defform[(template (var-id ...) form ...)]{

  Produces a @rtech{syntax transformer} that accepts one argument per
  @var[var-id] and then substitutes them into the @var[form]s.

  Example:
  @example[
    (define-syntax operate (template ($op) ($op 2 3 4)))
    (operate +)
    (operate *)
  ]
}

@defform[(define-template (id var-id ...) form ...)]{

  Creates a @rtech{transformer} binding of @var[id] with the @rtech{syntax
  transformer} produced by @racket[(template (var-id ...) form ...)].

  Example:
  @example[
    (define-template (operate $op) ($op 2 3 4))
    (operate +)
    (operate *)
  ]
}

@defform[(let-template ([(id var-id ...) form ...] ...) body ...)]{

  Creates a @rtech{transformer} binding of each @var[id] with the
  @rtech{syntax transformer} produced by @racket[(template (var-id ...) form
  ...)]. Each @var[id] is bound in the @var[body]s, and not in other
  @var[form]s.

  Example:
  @example[
    (let-template ([(fwd $x $y) '$x$y]
                   [(bwd $x $y) '$y$x])
      (list (fwd a b) (bwd a b)))
  ]
}

@defform[(splicing-let-template ([(id var-id ...) form ...] ...) body ...)]{

  Like @racket[let-template], except that in a definition context, the body
  forms are spliced into the enclosing definition context (in the same way as
  for @racket[begin]).

  Example:
  @example[
    (splicing-let-template ([(one) 1])
      (define o (one)))
    o
    (eval:error (one))
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

@defform[(define-template-ids id member-id ...)]{

  Defines @var[id] as a list of @rtech{identifiers} for use with
  @racket[in-template-ids].

}

@defproc[(in-template-ids [id identifier?]) sequence?]{

  Produces a sequence whose elements are the successive identifiers bound to
  @var[id] by @racket[define-template-ids].

  Example:
  @example[
    (define-template-ids operators + - * /)
    (for/template ([$op (in-template-ids #'operators)])
      (displayln ($op 2 3)))
  ]
}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@require[scribble/example]

@close-eval[template-evaluator]
