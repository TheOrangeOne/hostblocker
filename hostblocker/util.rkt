#lang racket

(provide (all-defined-out))


;; useful for debugging
(define (pp x)
  (pretty-print x)
  x)


;; http://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url
(define url-regex #px"(http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}([-a-zA-Z0-9@:%_\\+.~#?&//=]*)")


;; perform a confirmation produce `yes` if the user responds "y" else
;; produce `no`
(define (confirmation message yes no)
  (display (string-append message " [y/n] ") (current-output-port))
  (define response (read-line))
  (if (string=? response "y") yes no))
