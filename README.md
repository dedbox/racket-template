# Template Macros

[![Racket Package](https://img.shields.io/badge/raco%20pkg-template-red.svg)](https://pkgd.racket-lang.org/pkgn/package/template)
[![Documentation](https://img.shields.io/badge/read-docs-blue.svg)](http://docs.racket-lang.org/template/)
[![Build Status](https://travis-ci.org/dedbox/racket-template.svg?branch=master)](https://travis-ci.org/dedbox/racket-template)
[![Coverage Status](https://coveralls.io/repos/github/dedbox/racket-template/badge.svg?branch=master)](https://coveralls.io/github/dedbox/racket-template?branch=master)

## Dead Simple Racket Code Generation

Template macros tame the flexibility of [template
meta-programming](https://en.wikipedia.org/wiki/Template_metaprogramming) with
the safety of Racket's hygienic macro system.

Template variables are resolved *before* expansion by rewriting the input text
of identifiers or strings that contain template variables. The extra
flexibility makes escaping to the expanding environment less necessary *and*
more convenient.

``` racket
> (define-template (define-fibonaccis $max-n)
    (define-for-syntax fibonaccis
      (make-immutable-hash
       '(untemplate
         (for/fold ([fibs '([1 . 1] [0 . 0])])
                   ([n (in-range 2 (add1 $max-n))])
           (cons (cons n (+ (cdar fibs) (cdadr fibs))) fibs)))))
    (define/contract fibonacci
      (-> (integer-in 0 $max-n) exact-nonnegative-integer?)
      (curry hash-ref fibonaccis)))
> (define-fibonaccis 20)
> (fibonacci 10)
55
> (fibonacci 20)
6765
> (fibonacci 30)
; fibonacci: contract violation
;   expected: (integer-in 0 20)
;   given: 30
;   in: the 1st argument of
;       (-> (integer-in 0 20) natural?)
```

## Installation and Use

Template macros are distributed in the
[template](https://pkgs.racket-lang.org/package/template) package on the
official Racket package repository. It can be installed from DrRacket's
package manager, or with `raco pkg` from the comand line.

``` shell
raco pkg install template
```

To start using template macros, import the `template` collection.

``` racket
(require template)
```

## Contributing

Pull requests of any size are welcome. For help creating one, or to propose
major changes, please open an
[issue](https://github.com/dedbox/racket-template/issues/new) first to discuss
what you would like to change.
