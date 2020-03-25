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

(module+ test
  (require rackunit template (for-syntax racket/base))

  (test-case "#lang template"
    (local-require template/scribblings/lang-template)
    (the-template a 3)
    (check equal? as '(a a a))
    (check = a1 1) (check = a2 2) (check = a3 3))

  (module test-module-template template/lang
    ($x $n)
    (require (for-syntax racket/base))
    (define $xs '((for/template ([$_ (in-range 1 (add1 $n))]) $x)))
    (for/template ([$k (in-range 1 (add1 $n))])
      (define $x$k $k)))

  (test-case "module template/lang"
    (local-require 'test-module-template)
    (the-template b 5)
    (check equal? bs '(b b b b b))
    (check = b1 1) (check = b2 2) (check = b3 3)
    (check = b4 4) (check = b5 5))

  (test-case "load-template"
    (load-template template/scribblings/lang-template tpl)
    (tpl c 6)
    (check equal? cs '(c c c c c c))
    (check = c1 1) (check = c2 2) (check = c3 3)
    (check = c4 4) (check = c5 5) (check = c6 6)))
