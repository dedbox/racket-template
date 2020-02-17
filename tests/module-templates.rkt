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
  (require rackunit
           template)

  (test-case "#lang template"
    (local-require template/tests/lang-template)
    (the-template a)
    (check equal? as '(a a a)))

  (module test-module-template template/lang ($x)
    (define $xs '($x $x $x $x $x)))

  (local-require 'test-module-template)
  (the-template b)
  (check equal? bs '(b b b b b))

  (test-case "module template/lang"
    (local-require 'test-module-template)
    (the-template b)
    (check equal? bs '(b b b b b)))

  (test-case "load-template"
    (load-template tpl template/tests/lang-template)
    (tpl c)
    (check equal? cs '(c c c))))
