;; -*- mode: scheme; fill-column: 75; comment-column: 50; coding: utf-8; geiser-scheme-implementation: guile -*-

;; assoc that accepts a comparison predicate
(define (assoc key records equal?)
  (cond ((null? records) #f)
        ((equal? key (caar records))
         (car records))
        (else
         (assoc key (cdr records) equal?))))
