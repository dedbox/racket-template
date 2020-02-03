#lang racket/base

(provide (all-defined-out))

(define template-readtable
  (make-readtable
   (current-readtable) #\/ 'dispatch-macro
   (case-lambda
     [(ch port) `(unquote-template ,(read/recursive port))]
     [(ch port src line col pos)
      (datum->syntax #f
                     `(unquote-template ,(read-syntax/recursive src port))
                     (let-values ([(l c p) (port-next-location port)])
                       (list src line col pos (and pos (- p pos)))))])))
