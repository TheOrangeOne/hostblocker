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
     "# this is a comment"
     ""
     "0.0.0.0 ad-g.doubleclick.net       #! ads crap"
     "0.0.0.0 adsense.com                #! ads"))
(define simple-hf (los->input-port simple-hf-los))


(test-case
    "hostsfile-parse: simple hostsfile"
  (define hf (hostsfile-parse simple-hf))
  (define-values (lines hosts tags sources numline) (hostsfile-values hf))

  ;(check-equal? (gvector->list lines) simple-hf-los)

  (check-false (hostsfile-has-host? hf "facebook.com"))
  (check-true (hostsfile-has-host? hf "adsense.com"))
  (check-true (hostsfile-has-host? hf "ad-g.doubleclick.net"))

  (check-equal? (hostsfile-host-tags hf "adsense.com")
                '("ads"))
  (check-equal? (hostsfile-host-tags hf "ad-g.doubleclick.net")
                '("ads" "crap"))


  (define tags1 (hostsfile-tag-hosts hf "ads"))
  (check-true (in-list? tags1 "adsense.com"))

  (check-true (hostsfile-has-source? hf "http://adaway.org/hosts.txt"))


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

