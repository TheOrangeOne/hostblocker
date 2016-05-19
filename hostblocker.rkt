#lang racket

(require racket/cmdline)
(require net/http-client)
(require net/head)
(require net/url)


(define-struct loe (entries))

;; (pip->lost pipe) -> (listof string)
;; convert an input pipe to a list of string
(define (pipe->los pipe)
  (define line (read-line pipe))
  (if (eof-object? line)
      empty
      (cons line (pipe->los pipe))))

;; (fetch-hostfile url) -> (listof string)
;; GET hostfile located at url
(define (get-hostfile url)
  (with-handlers
    ([(λ (x) #t)
      (λ (x) (error (string-append "ERRORi could not connect to " url)))])
    (define-values (status header resp)
      (http-sendrecv/url (string->url url) #:method "GET"))
    (pipe->los resp)))


(define hosts-file (make-parameter "/etc/hosts"))

;; define commandline
(define file
  (command-line
   #:program "hostblocker"
   #:once-each
   [("-f" "--file") filename
                    "specify hosts file: <filename>"
                    (hosts-file filename)]
   #:once-each
   [("-g" "--get") location
                   "gets the host file at <location>"
    (void (map displayln (get-hostfile location)))]
   #:args (filename) (hosts-file filename)))
