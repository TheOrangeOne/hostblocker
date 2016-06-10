#lang racket

(require
 "lib.rkt"
 data/gvector
 rackunit)

(require "hostsfile.rkt")


(define sample-hf-los
  '( "0.0.0.0 facebook.com"
     "0.0.0.0 twitter.com"
     "0.0.0.0 instagram.com"
     ))
(define sample-hf (los->input-port sample-hf-los))


(define simple-hf-los
  '( "#! hostblocker srcs:"
     "#!  http://adaway.org/hosts.txt"
     "#!  /home/fred/downloads/hosts"
     "#! end srcs"
     "# this is a sample hostsfile produced by hostblocker"
     "# this is a localhost entry -- we don't want to mess with it"
     "127.0.0.1 localhost local.host orange"
     ""
     "     "
     "# this is a comment"
     "0.0.0.0 ad-g.doubleclick.net       #! ads crap"
     "0.0.0.0 adsense.com                #! ads"))
(define simple-hf (los->input-port simple-hf-los))


(test-case
    "hostsfile-parse: simple hostsfile"
  (define hf (hostsfile-parse simple-hf))
  (define-values (lines hosts tags sources numline) (hostsfile-values hf))

  (check-false (hostsfile-has-host? hf "facebook.com"))
  (check-true (hostsfile-has-host? hf "adsense.com"))
  (check-true (hostsfile-has-host? hf "ad-g.doubleclick.net"))

  (check-equal? (hostsfile-host-tags hf "adsense.com")
                '("ads"))
  (check-equal? (hostsfile-host-tags hf "ad-g.doubleclick.net")
                '("ads" "crap"))


  (define tags1 (hostsfile-tag-hosts hf "ads"))
  (check-true (in-list? tags1 "adsense.com"))
  (check-true (in-list? tags1 "ad-g.doubleclick.net"))

  (define tags2 (hostsfile-tag-hosts hf "crap"))
  (check-true (in-list? tags2 "ad-g.doubleclick.net"))

  (check-true (hostsfile-has-source? hf "http://adaway.org/hosts.txt"))
  (check-true (hostsfile-has-source? hf "/home/fred/downloads/hosts"))


  )
