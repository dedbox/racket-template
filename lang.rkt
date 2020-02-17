#lang racket/base

(require racket/base template)

(provide (except-out (all-from-out racket/base) #%module-begin)
         (all-from-out template)
         (rename-out [template-module-begin #%module-begin]))
