#lang racket/base

(require gregor
         gregor/period
         racket/contract
         racket/format
         racket/string
         "error.rkt")

;; contracts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 maybe-string/c
 maybe-moment/c
 maybe-period/c)

(define maybe-string/c (or/c false/c non-empty-string?))
(define maybe-moment/c (or/c false/c moment?))
(define maybe-period/c (or/c false/c time-period?))


;; xml ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 children-by-tag
 ref-child
 optional
 ->timestamp
 ->ttl)

(define (children-by-tag e t)
  (for/list ([c (in-list e)]
             #:when (and (pair? c)
                         (eq? (car c) t)))
    c))

(define ref-child
  (case-lambda
    [(elt name)
     (ref-child elt name (lambda ()
                           (rss-error "expected a field named ~a" name)))]

    [(elt name default)
     (or (for/first ([child (in-list (children-by-tag elt name))])
           ;; FIXME: Handle symbols.
           (case (length child)
             [(2) (cadr child)]
             [(3) (caddr child)]
             [else (apply string-append
                          (for/list ([c (in-list (cddr child))])
                            (cond
                              [(string? c) c]
                              [(integer? c) (string (integer->char c))]
                              [else (~a c)])))]))
         (if (procedure? default)
             (default)
             default))]))

(define (optional tag v)
  (if v `((,tag ,v)) null))

(define (->ttl t)
  (and t (number->string (period-ref t 'minutes))))

(define (->timestamp m)
  (and m (~t m "EEE, dd MMM yyyy HH:mm:ss XX")))


;; parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 parse-timestamp
 parse-ttl)

(define (parse-timestamp s)
  (parse-moment s "EEE, dd MMM yyyy HH:mm:ss XX"))

(define (parse-ttl s)
  (minutes (string->number s)))
