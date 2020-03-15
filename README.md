# Template Macros

[![Racket Package](https://img.shields.io/badge/raco%20pkg-template-red.svg)](https://pkgd.racket-lang.org/pkgn/package/template)
[![Documentation](https://img.shields.io/badge/read-docs-blue.svg)](http://docs.racket-lang.org/template/)
[![Build Status](https://travis-ci.org/dedbox/racket-template.svg?branch=master)](https://travis-ci.org/dedbox/racket-template)
[![Coverage Status](https://coveralls.io/repos/github/dedbox/racket-template/badge.svg?branch=master)](https://coveralls.io/github/dedbox/racket-template?branch=master)

## Dead Simple Code Generation for Racket

Template macros combine the flexibility of [template
meta-programming](https://en.wikipedia.org/wiki/Template_metaprogramming) with
the safety of Racket's hygienic macro system.

Template variables are resolved *before* expansion by selectively rewriting
the input text. The extra flexibility makes escaping to the expanding
environment less necessary *and* more convenient.

``` racket
#lang racket/base

(require racket/match template (for-syntax racket/base))

(define-for-syntax the-fibs
  (make-immutable-hash
   (for/fold ([fibs '([1 . 1] [0 . 0])])
             ([k (in-range 2 10)])
     `([,k . ,(+ (cdar fibs) (cdadr fibs))] ,@fibs))))

(begin-template '#,(map cdr (sort (hash->list the-fibs) < #:key car)))

; '(0 1 1 2 3 5 8 13 21 34)

(begin-template
  (define fib (match-lambda (for/template ([K (in-range 10)])
                              [K #,(hash-ref the-fibs K)]))))

(fib 8)

; 21

(fib 10)

; match-lambda: no matching clause for 10
; /tmp/f.rkt:15:14
; Context:
;  /usr/share/racket/collects/racket/match/runtime.rkt:24:0 match:error
;  "/tmp/f.rkt":1:1 [running body]
; [Due to errors, REPL is just module language, requires, and stub definitions]
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

See the [official documentation](https://docs.racket-lang.org/template/) for a
detailed overview of template macros, along with a catalog of template
constructors, combiners, and definers.

## Contributing

Pull requests of any size are welcome. For help creating one, or to propose
major changes, please open an
[issue](https://github.com/dedbox/racket-template/issues/new) first to discuss
what you would like to change.
