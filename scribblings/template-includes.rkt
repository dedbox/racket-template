#lang racket/base

(require racket/sandbox
         scribble/example
         scribble/manual)

(provide (all-defined-out))

(define (rtech . args)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") args))

(define template-evaluator
  (call-with-trusted-sandbox-configuration
   (λ ()
     (parameterize ([sandbox-output       'string]
                    [sandbox-error-output 'string])
       (make-base-eval #:lang 'racket/base '(void)))))) 

(define-syntax-rule (example expr ...)
  (examples #:eval template-evaluator #:label #f expr ...))

(void (example #:hidden (require template
                                 (for-syntax racket/base
                                             racket/sequence))))
