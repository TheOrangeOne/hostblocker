#lang racket

;; this module is meant to provide useful functions
;; of all sorts

(provide (all-defined-out))


;; http://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url
(define url-regex #px"(http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}([-a-zA-Z0-9@:%_\\+.~#?&//=]*)")

;; (error-text msg) -> string?
;;   msg: string?
;;
;; return an error message
(define (error-text msg)
  (format "ERROR: ~a" msg))


;; (string-empty? s) -> boolean?
;;   s: string?
;;
;; return true if the string is empty
(define (string-empty? s)
  (string=? "" s))

;; (pip->lost pipe) -> (listof string)
;;   pipe: input-port?
;;
;; take an input pipe and produce the corresponding
;; list of string
(define (pipe->los pipe)
  (define line (read-line pipe))
  (if (eof-object? line)
      empty
      (cons line (pipe->los pipe))))
