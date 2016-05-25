#lang racket

(require
 rackunit
 srfi/1)

(require "hostblocker.rkt")


(define (in-list? val lst)
  (if (list-index (curry equal? val) lst) #t #f))


;; TODO find a better way to test hash-maps
(test-case
    "generate-sources simple hostsfile"

  (define hosts-file-los
    '( "#! src: http://adaway.org/hosts.txt"
       "0.0.0.0 ad-g.doubleclick.net       #! ads crap"
       "0.0.0.0 adsense.com                #! ads"
       "#! end src"))
  (define sources (generate-sources hosts-file-los))
  (check-true (hash? sources) "Test passed")
  (check-true (hash-has-key? sources "http://adaway.org/hosts.txt"))

  (define adawaysrc (hash-ref sources "http://adaway.org/hosts.txt"))
  (define entries (hash-map adawaysrc (λ (k v) k)))

  (check-true (in-list? "ad-g.doubleclick.net" entries))
  (check-true (in-list? "adsense.com" entries))
  (check-equal? (hash-ref adawaysrc "ad-g.doubleclick.net") '("ads" "crap"))
  (check-equal? (hash-ref adawaysrc "adsense.com") '("ads")))

(test-case
    "generate-sources multiple sources hostsfile"

  (define hosts-file-los
    '( "#! src: http://adaway.org/hosts.txt"
       "0.0.0.0 ad-g.doubleclick.net       #! ads crap"
       "0.0.0.0 adsense.com                #! ads"
       "#! end src"
       ""
       " "
       "             "
       "#! src: /home/user/myhostsfile"
       "0.0.0.0 facebook.com               #! garbage crap"
       "0.0.0.0 twitter.com                #! useless trash"
       "0.0.0.0 reddit.com"
       "#! end src"))
  (define sources (generate-sources hosts-file-los))
  (check-true (hash? sources))
  (check-true (hash-has-key? sources "http://adaway.org/hosts.txt"))

  (define adawaysrc (hash-ref sources "http://adaway.org/hosts.txt"))
  (define entries (hash-map adawaysrc (λ (k v) k)))

  (check-true (in-list? "ad-g.doubleclick.net" entries))
  (check-true (in-list? "adsense.com" entries))
  (check-equal? (hash-ref adawaysrc "ad-g.doubleclick.net") '("ads" "crap"))
  (check-equal? (hash-ref adawaysrc "adsense.com") '("ads"))


  (check-true (hash-has-key? sources "/home/user/myhostsfile"))

  (define mhf-src (hash-ref sources "/home/user/myhostsfile"))
  (define mhf-entries (hash-map mhf-src (λ (k v) k)))

  (check-true (in-list? "facebook.com" mhf-entries))
  (check-true (in-list? "reddit.com" mhf-entries))
  (check-equal? (hash-ref mhf-src "facebook.com") '("garbage" "crap"))
  (check-equal? (hash-ref mhf-src "twitter.com") '("useless" "trash"))
  (check-equal? (hash-ref mhf-src "reddit.com") '()))
