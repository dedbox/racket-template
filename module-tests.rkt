#lang racket/base

(module+ test
  (require rackunit
           template)

  (test-case "#lang template"
    (local-require template/lang-test)
    (the-template a)
    (check equal? as '(a a a)))

  (module test-module-template template ($x)
    (define $xs '($x $x $x $x $x)))

  (test-case "module template"
    (local-require 'test-module-template)
    (the-template b)
    (check equal? bs '(b b b b b)))

  (test-case "load-template-module"
    (load-template-module tpl template/lang-test)
    (tpl c)
    (check equal? cs '(c c c))))
