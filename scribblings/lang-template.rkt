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

#lang template ($x $n)

(require (for-syntax racket/base))

(define $xs '((for/template ([$_ (in-range 1 (add1 $n))]) $x)))

(for/template ([$k (in-range 1 (add1 $n))])
  (define $x$k $k))
