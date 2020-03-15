#lang racket/base

(require gregor
         gregor/period
         racket/contract
         racket/string
         threading
         xml
         "common.rkt"
         "rss-item.rkt")

(provide
 xexpr->rss-channel
 rss-channel->xexpr
 (struct-out rss-channel))

(struct/contract
 rss-channel
 ([link non-empty-string?]
  [title non-empty-string?]
  [language maybe-string/c]
  [description non-empty-string?]
  [generator maybe-string/c]
  [published-at maybe-moment/c]
  [last-built-at maybe-moment/c]
  [ttl maybe-period/c]
  [items (listof rss-item?)])
  #:transparent)

(define/contract (xexpr->rss-channel e)
  (-> xexpr? rss-channel?)
  (define-syntax-rule (ref name default ...)
    (ref-child e 'name default ...))

  (rss-channel
   (ref link)
   (ref title)
   (ref language #f)
   (ref description)
   (ref generator #f)
   (and~> (ref pubDate #f) parse-timestamp)
   (and~> (ref lastBuildDate #f) parse-timestamp)
   (and~> (ref ttl #f) parse-ttl)
   (map xexpr->rss-item (children-by-tag e 'item))))

(define/contract (rss-channel->xexpr ch)
  (-> rss-channel? xexpr?)
  `(channel
    (link ,(rss-channel-link ch))
    (title ,(rss-channel-title ch))
    ,@(optional 'language (rss-channel-language ch))
    (description ,(rss-channel-description ch))
    ,@(optional 'generator (rss-channel-generator ch))
    ,@(optional 'pubDate (->timestamp (rss-channel-published-at ch)))
    ,@(optional 'lastBuildDate (->timestamp (rss-channel-last-built-at ch)))
    ,@(optional 'ttl (->ttl (rss-channel-ttl ch)))
    ,@(map rss-item->xexpr (rss-channel-items ch))))

(module+ test
  (require rackcheck
           rackunit)

  (define gen:moment
    (gen:let ([t (gen:integer-in (->posix (date 1995 1 1))
                                 (->posix (today)))])
      (define dt (posix->datetime t))
      (moment (->year dt)
              (->month dt)
              (->day dt)
              (->hours dt)
              (->minutes dt)
              (->seconds dt)
              (->nanoseconds dt)
              #:tz 0)))

  (define (gen:optional g)
    (gen:choice (gen:const #f) g))

  (define gen:non-empty-string
    (gen:no-shrink
     (gen:let ([hd gen:char-letter]
               [tl (gen:string gen:char-letter #:max-length 20)])
       (apply string hd (string->list tl)))))

  (define gen:rss-item
    (gen:let ([link gen:non-empty-string]
              [title gen:non-empty-string]
              [guid (gen:optional gen:non-empty-string)]
              [description gen:non-empty-string]
              [published-at (gen:optional gen:moment)])
      (rss-item
       link
       title
       guid
       description
       published-at)))

  (define gen:rss-channel
    (gen:let ([link gen:non-empty-string]
              [title gen:non-empty-string]
              [language (gen:one-of '(#f "en-US" "en-GB" "fr-FR" "ro-RO"))]
              [description gen:non-empty-string]
              [generator gen:non-empty-string]
              [published-at (gen:optional gen:moment)]
              [last-built-at (gen:optional gen:moment)]
              [ttl (gen:optional gen:natural)]
              [items (gen:list gen:rss-item #:max-length 20)])
      (rss-channel
       link
       title
       language
       description
       generator
       published-at
       last-built-at
       (and ttl (minutes ttl))
       items)))

  (define-property
    there-and-back-again
    ([ch gen:rss-channel])
    (check-equal? (xexpr->rss-channel (rss-channel->xexpr ch)) ch))

  (check-property there-and-back-again))
