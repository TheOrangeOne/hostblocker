#lang racket

(require
 "lib.rkt"
 rackunit)

(require "hostblocker.rkt")


(define sample-hf-los
  '( "0.0.0.0 facebook.com"
     "0.0.0.0 twitter.com"
     "0.0.0.0 instagram.com"
     ))
(define sample-hf (los->input-port sample-hf-los))


(define simple-hf-los
  '( "# this is a sample hostsfile produced by hostblocker"
     "# this is a comment"
     ""
     "#! src: http://example.com/hosts.txt"
     "0.0.0.0 ad-g.doubleclick.net       #! ads crap"
     "0.0.0.0 adsense.com                #! ads"
     "#! end src"))
(define simple-hf (los->input-port simple-hf-los))



(test-case
    "hostsfile-parse: simple hostsfile"
  (define hf (hostsfile-parse simple-hf))
  (define-values (entries sources tags orig) (hostsfile-values hf))

  (check-true (hash-has-key? sources "http://example.com/hosts.txt"))

  ;; check entries
  (define src-entries (hostsfile-get-entries hf "http://example.com/hosts.txt"))
  (check-true (hash-has-key? src-entries "ad-g.doubleclick.net"))
  (check-true (hash-has-key? src-entries "adsense.com"))

  ;; check tags of entries
  (check-equal? '("ads")
                (hostsfile-source-entry-tags
                 hf "http://example.com/hosts.txt" "adsense.com"))

  (check-equal? '("ads" "crap")
                (hostsfile-source-entry-tags
                 hf "http://example.com/hosts.txt" "ad-g.doubleclick.net"))

  ;; check tags
  (check-true (hostsfile-has-tag? hf "crap"))
  (check-true (hostsfile-has-tag? hf "ads"))
  )




(define multiple-srcs-hf-los
  '( "#! src: http://example.com/hosts.txt"
     "0.0.0.0 ad-g.doubleclick.net       #! ads crap"
     "0.0.0.0 adsense.com                #! ads"
     "#! end src"
     ""
     " "
     "127.0.0.1 localhost"
     "             "
     "#! src: /home/user/myhostsfile"
     "0.0.0.0 facebook.com               #! garbage crap"
     "0.0.0.0 twitter.com                #! useless trash"
     "0.0.0.0 reddit.com"
     "#! end src"))
(define multiple-srcs-hf (los->input-port multiple-srcs-hf-los))


(test-case
    "hostsfile-parse multiple sources hostsfile"
  (define hf (hostsfile-parse multiple-srcs-hf))
  (define-values (entries sources tags orig) (hostsfile-values hf))

  (check-true (hash-has-key? sources "http://example.com/hosts.txt"))
  (check-true (hash-has-key? sources "/home/user/myhostsfile"))

  ;; check entries

  (define src1-entries
    (hostsfile-get-entries hf "http://example.com/hosts.txt"))

  (check-true (hash-has-key? src1-entries "ad-g.doubleclick.net"))
  (check-true (hash-has-key? src1-entries "adsense.com"))

  (define src2-entries
    (hostsfile-get-entries hf "/home/user/myhostsfile"))
  (check-true (hash-has-key? src2-entries "facebook.com"))
  (check-true (hash-has-key? src2-entries "twitter.com"))
  (check-true (hash-has-key? src2-entries "reddit.com"))


  ;; check tags of entries
  (check-equal? '("ads" "crap")
                (hostsfile-source-entry-tags
                 hf "http://example.com/hosts.txt" "ad-g.doubleclick.net"))
  (check-equal? '("ads")
                (hostsfile-source-entry-tags
                 hf "http://example.com/hosts.txt" "adsense.com"))


  ;; check tags
  (check-true (hostsfile-has-tag? hf "crap"))
  (check-true (hostsfile-has-tag? hf "ads"))
  )
