#lang racket/base

(require racket/contract
         racket/string
         threading
         xml
         "common.rkt")

(provide
 xexpr->rss-item
 rss-item->xexpr
 (struct-out rss-item))

(struct/contract
 rss-item
 ([link non-empty-string?]
  [title non-empty-string?]
  [guid maybe-string/c]
  [description non-empty-string?]
  [published-at maybe-moment/c])
 #:transparent)

(define/contract (xexpr->rss-item e)
  (-> xexpr? rss-item?)
  (define-syntax-rule (ref name default ...)
    (ref-child e 'name default ...))

  (rss-item
   (ref link)
   (ref title)
   (ref guid #f)
   (ref description)
   (and~> (ref pubDate #f) parse-timestamp)))

(define/contract (rss-item->xexpr it)
  (-> rss-item? xexpr?)
  `(item
    (link ,(rss-item-link it))
    (title ,(rss-item-title it))
    ,@(optional 'guid (rss-item-guid it))
    (description ,(rss-item-description it))
    ,@(optional 'pubDate (->timestamp (rss-item-published-at it)))))
