#lang racket

(require racket/cmdline)
(require net/http-client)
(require net/head)
(require net/url)

(error-print-context-length 0)


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
(define (get-remote-hostfile url)
  (with-handlers
    ([(λ (x) #t)
      (λ (x) (error (string-append "ERROR: could not connect to " url)))])
    (define-values (status header resp)
      (http-sendrecv/url (string->url url) #:method "GET"))
    (pipe->los resp)))

(define (read-hostsfile file)
  (define los (pipe->los file))
  (void (map displayln los)))


(define (main)
  (define hostsfile (open-input-file (hostsfile-path)))
  (read-hostsfile hostsfile))


(define hostsfile-path (make-parameter "/etc/hosts"))
(define remote-src (make-parameter ""))

;; define commandline
(define cmd
  (command-line
   #:program "hostblocker"
   #:once-each
   [("-f" "--file") filename
                    "specify hosts file: <filename>"
                    (hostsfile-path filename)]
   [("-g" "--get") location
                   "gets the host file at <location> and adds it as a source"
                   (remote-src location)]))

(main)
