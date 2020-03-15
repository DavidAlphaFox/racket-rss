#lang racket/base

(require net/url
         racket/contract
         racket/match
         racket/port
         xml
         "private/common.rkt"
         "private/error.rkt"
         "private/rss-channel.rkt"
         "private/rss-item.rkt")

(provide
 exn:fail:rss?
 read-feed
 read-feed-url
 (struct-out rss-channel)
 (struct-out rss-item))

(define/contract (read-feed [in (current-input-port)])
  (->* () (input-port?) (listof rss-channel?))
  (define e
    (xml->xexpr
     (document-element
      (read-xml in))))
  (map xexpr->rss-channel (children-by-tag e 'channel)))

(define/contract (read-feed-url url-or-string)
  (-> (or/c url? string?) (listof rss-channel?))
  (define-values (status headers in)
    (http-sendrecv/url
     (if (url? url-or-string)
         url-or-string
         (string->url url-or-string))))

  (match status
    [(regexp #rx#"^HTTP.... 200 ")
     (read-feed in)]

    [_
     (error 'read-feed-url
            "unexpected response\n  status: ~.s\n  headers: ~.s\n  body: ~.s"
            status
            headers
            (port->string in))]))
