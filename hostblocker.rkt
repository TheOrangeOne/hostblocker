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

;; (fetch-hostsfile url) -> (listof string)
;; GET hostsfile located at url
(define (get-remote-hostsfile url)
  (with-handlers
    ([(λ (x) #t)
      (λ (x) (error (string-append "ERROR: could not connect to " url)))])
    (define-values (status header resp)
      (http-sendrecv/url (string->url url) #:method "GET"))
    (pipe->los resp)))

(define (parse-hostsfile file)
  (define los (pipe->los file))
  (void (map displayln los)))

(define (is-source? line)
  (define l (string-split line))
  (if (and (> (length l) 2)
           (string=? (first l) "#!")
           (string=? (second l) "src:"))
      (third l)
      #f))

(define (get-source line)
  (third (string-split line)))

(define (read-sources hf srcs)
  (define line (read-line hf))
  (cond [(string=? line "#! end cookie")
         srcs]
        [(is-source? line)
         (hash-set! srcs (get-source line) (make-hash))
         (read-sources hf srcs)]))


;; (read-cookie hf) -> (hash)
;;   hf: input-pipe
;; produce a hash with the keys being the sources
;; listed in the cookie
;; note: the corresponding value for the keys is an empty (hash)
;;       to be used for adding entries later on
(define (read-cookie hf)
  (define line (read-line hf))
  (cond [(string=? line "#! begin cookie")
         (read-sources hf (make-hash))]
        [else (read-cookie hf)]))


(define (get-groups split-line)
  (rest (rest (rest split-line))))

(define (add-entry src-hash line)
  (define split (string-split line))
  (define entry (string-append (first split) " " (second split)))
  (hash-set! src-hash entry (get-groups split)))

;; (populate-source hf src-hash srcs) -> (read-body hf srcs)
;;   hf: input-pipe
;;   src-hash: hash
;;   srcs: hash
(define (populate-source hf src-hash srcs)
  (define line (read-line hf))
  (cond [(string=? line "#! end src")
         (read-body hf srcs)]
        [else
         ;(hash-set! src-hash line '())
         (add-entry src-hash line)
         (populate-source hf src-hash srcs)]))

(define (read-body hf srcs)
  (define line (read-line hf))
  (cond [(eof-object? line) srcs]
        [(is-source? line)
         (define src (get-source line))
         (define src-hash
           (hash-ref srcs src
                     (λ () (error (string-append "ERROR: "src" is not a defined source")))))
         (read-body hf (populate-source hf src-hash srcs))]
        [else (read-body hf srcs)]))

(define (main)
  (define hostsfile (open-input-file (hostsfile-path)))
  (define sources (read-cookie hostsfile))
  (define popped-sources (read-body hostsfile sources))
  (pretty-print popped-sources))


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
