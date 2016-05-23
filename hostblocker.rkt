#lang racket

(require racket/cmdline)
(require net/http-client)
(require net/head)
(require net/url)

(error-print-context-length 0)


(define (log line)
  (if (logging) (displayln line) (void)))

;; (pip->lost pipe) -> (listof string)
;; convert an input pipe to a list of string
(define (pipe->los pipe)
  (define line (read-line pipe))
  (if (eof-object? line)
      empty
      (cons line (pipe->los pipe))))


;; (fetch-hostsfile url) -> (input-pipe)
;; GET hostsfile located at url
(define (get-remote-hostsfile url)
  (with-handlers
    ([(λ (x) #t)
      (λ (x) (error (string-append "ERROR: could not connect to " url)))])
    (define-values (status header resp)
      (http-sendrecv/url (string->url url) #:method "GET"))
    resp))

;; (fetch-hostsfile url) -> (listof string)
;; GET hostsfile located at url
(define (get-remote-hostsfile-los url)
  (with-handlers
    ([(λ (x) #t)
      (λ (x) (error (string-append "ERROR: could not connect to " url)))])
    (define-values (status header resp)
      (http-sendrecv/url (string->url url) #:method "GET"))
    (pipe->los resp)))


(define (get-local-hostsfile path)
  (with-handlers
    ([(λ (x) #t)
      (λ (x) (error (string-append "ERROR: could not open or find file " path)))])
    (open-input-file path)))

(define (get-local-hostsfile-los path)
  (with-handlers
    ([(λ (x) #t)
      (λ (x) (error (string-append "ERROR: could not open or find file " path)))])
    (pipe->los (open-input-file path))))


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
  (if (and (> (length split-line) 2) (string=? (third split-line) "#!"))
      (rest (rest (rest split-line)))
      '()))

(define (add-entry src-hash line)
  (define split (string-split line))
  (define entry (string-append "0.0.0.0 " (second split)))
  (hash-set! src-hash entry (get-groups split)))


(define (is-entry? line)
  (and (> (string-length line) 0) (not (char=? (string-ref line 0) #\#))))

;; (populate-source hf src-hash srcs) -> (read-body hf srcs)
;;   hf: input-pipe
;;   src-hash: hash
;;   srcs: hash
(define (populate-source hf src-hash srcs)
  (define line (read-line hf))
  (cond [(string=? line "#! end src")
         (read-body hf srcs)]
        [(is-entry? line)
         (add-entry src-hash line)
         (populate-source hf src-hash srcs)]
        [else (populate-source hf src-hash srcs)]))

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


;; (list-sources) -> (void)
;; requires:
;;    parameters: (hostsfile-path)
;; side-effects:
;;   output the sources listed in the cookie section
;;   of the hostsfile
(define (list-sources)
  (define hostsfile (open-input-file (hostsfile-path)))
  (define sources (read-cookie hostsfile))
  (displayln (string-append "sources for hostfile " (hostsfile-path) ":"))
  (void (hash-for-each sources (λ (k v) (displayln (string-append "  " k))))))

;; (list-entries source) -> (void)
;; requires:
;;   parameters: (hostsfile-path)
;; side-effects:
;;   output the entries for a given source `source`
(define (list-entries source)
  (define hostsfile (open-input-file (hostsfile-path)))
  (define sources (read-cookie hostsfile))
  (define popped-sources (read-body hostsfile sources))
  (define source-hash
    (hash-ref popped-sources source
              (λ () (error (string-append "ERROR: source not found in " (hostsfile-path))))))
  (displayln (string-append "entries for source " source ":"))
  (void (hash-for-each source-hash (λ (k v) (displayln (string-append "  " k))))))

;; http://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url
(define url-regex #px"(http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}([-a-zA-Z0-9@:%_\\+.~#?&//=]*)")

(define (read-source src-file)
  (if (regexp-match? url-regex src-file)
      (get-remote-hostsfile src-file)
      (get-local-hostsfile src-file)))

(define (generate-entries newsrc-pipe sources [entries (make-hash)])
  (define line (read-line newsrc-pipe))
  (cond [(eof-object? line) entries]
        [(is-entry? line)
         (add-entry entries line)
         (generate-entries newsrc-pipe sources entries)]
        [else (generate-entries newsrc-pipe sources entries)]))


(define (add-source newsrc sources)
  (cond [(not (string=? newsrc ""))
         (if (hash-has-key? sources newsrc)
             (error "ERROR: source already exists")  ; TODO add prompt to overwrite
             (void))
         (define newsrc-pipe (read-source newsrc))
         (hash-set! sources newsrc (generate-entries newsrc-pipe sources))
         (void)]
        [else (void)]))


(define (main)
  (define hostsfile (open-input-file (hostsfile-path)))
  (define sources (read-cookie hostsfile))
  (read-body hostsfile sources)            ; read body of hostsfile, populate sources
  (add-source (new-source) sources))


(define hostsfile-path (make-parameter "/etc/hosts"))
(define new-source (make-parameter ""))
(define hostsfile-out (make-parameter (hostsfile-path)))
(define logging (make-parameter #t))

;; define commandline
(define cmd
  (command-line
   #:program "hostblocker"
   #:once-each
   [("-a" "--add") source
    "add a local or remote source: <source>"
    (new-source source)]
   [("-l" "--list")
    "list known sources in the hostfile specified"
    (list-sources)]
   [("-e" "--entries") source
    "list entries of a source: <source>"
    (list-entries source)]
   [("-f" "--file") filename
    "specify hosts file: <filename>"
    (hostsfile-path filename)]
   [("-v" "--verbose")
    "display logging info"
    (logging #t)]
   [("-o" "--out") filename
    "specify output hosts file: <filename>"
    (hostsfile-out filename)]))
(main)
