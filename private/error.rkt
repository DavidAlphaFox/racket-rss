#lang racket/base

(provide
 exn:fail:rss?
 rss-error)

(struct exn:fail:rss exn:fail ())

(define (rss-error message . args)
  (raise (exn:fail:rss (apply format message args) (current-continuation-marks))))
