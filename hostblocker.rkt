#lang racket

(require
 racket/cmdline
 net/http-client
 net/head
 net/url
 rackunit)

(error-print-context-length 0) ;; prevent stacktrace


(provide (all-defined-out))


;; TODO: document or clean up
(define (log line)
  (if (logging) (displayln line) (void)))


(define (string-empty? s)
  (string=? "" s))


(define (error-text msg)
  (format "ERROR: ~a" msg))


;; (pip->lost pipe) -> (listof string)
;; convert an input pipe to a list of string
;; TODO: document or clean up
(define (pipe->los pipe)
  (define line (read-line pipe))
  (if (eof-object? line)
      empty
      (cons line (pipe->los pipe))))


;; (fetch-hostsfile url) -> (input-pipe)
;; GET hostsfile located at url
;; TODO: document or clean up
(define (get-remote-hostsfile url)
  (with-handlers
    ([(λ (x) #t)
      (λ (x) (error (error-text (format "could not connect to ~a" url))))])
    (define-values (status header resp)
      (http-sendrecv/url (string->url url) #:method "GET"))
    resp))


;; (fetch-hostsfile url) -> (listof string)
;; GET hostsfile located at url
;; TODO: document or clean up
(define (get-remote-hostsfile-los url)
  (pipe->los (get-remote-hostsfile url)))


;; TODO: document or clean up
(define (get-local-hostsfile path)
  (with-handlers
    ([(λ (x) #t)
      (λ (x) (error (error-text (format "could not open or find file '~a'" path))))])
    (open-input-file path)))


;; TODO: document or clean up
(define (get-local-hostsfile-los path)
  (pipe->los (get-local-hostsfile path)))


;; TODO: document or clean up
(define (parse-hostsfile file)
  (define los (pipe->los file))
  (void (map displayln los)))


;; http://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url
(define url-regex #px"(http(s)?:\\/\\/.)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,6}([-a-zA-Z0-9@:%_\\+.~#?&//=]*)")


;; TODO: document or clean up
(define (read-source src-file)
  (if (regexp-match? url-regex src-file)
      (get-remote-hostsfile src-file)
      (get-local-hostsfile src-file)))


;; TODO: document or clean up
(define (generate-entries newsrc-pipe sources [entries (make-hash)])
  (define line (read-line newsrc-pipe))
  (cond [(eof-object? line) entries]
        [(is-entry? line)
         (add-entry entries line)
         (generate-entries newsrc-pipe sources entries)]
        [else (generate-entries newsrc-pipe sources entries)]))


;; TODO: document or clean up
(define (add-source newsrc sources)
  (cond [(not (string=? newsrc ""))
         (if (hash-has-key? sources newsrc)
             (error (error-text (format "source '~a' already exists" newsrc)))
             (void))
         (define newsrc-pipe (read-source newsrc))
         (hash-set! sources newsrc (generate-entries newsrc-pipe sources))
         (void)]
        [else (void)]))


;; TODO: document or clean up
(define (write-hostsfile newsrc sources out)
  (cond [(not (string=? newsrc ""))
         (fprintf out "~n")
         (fprintf out (string-append "#! src: " newsrc "~n"))
         (fprintf out "#! end src~n")]
        [else (void)]))


;; (list-entries src srcs-hash) -> (void)
;;   src: string?
;;   srcs-hash: hash?
;;
;; requires:
;;   parameters: (hostsfile-path)
;; side-effects:
;;   output the entries for a given source hash `srcs-hash`
(define (list-entries src srcs-hash)
  (define entries (get-entries src srcs-hash))
  (displayln (format "entries for source ~a:" src))
  (void (map (λ (x) (displayln (format  "  ~a" x))) entries)))


;; (get-entries src srcs-hash) -> (listof string?)
;;   src: string?
;;   src-hash: hash?
;;
;; requires:
;;   parameters: (hostsfile-path)
;;
;; return the entries for a given source `src`
(define (get-entries src srcs-hash)
  (define errtxt
    (error-text (format "source '~a' not found in ~a" src (hostsfile-path))))
  (define source-hash
    (hash-ref srcs-hash src
              (λ () (error errtxt))))
  (hash-map source-hash (λ (k v) k)))


;; (list-sources srcs-hash) -> (void)
;;   srcs-hash: hash?
;;
;; requires:
;;   parameters: (hostsfile-path)
;; side-effects:
;;   print out the sources given the sources hash
(define (list-sources srcs-hash)
  (define sources (get-sources srcs-hash))
  (displayln (format "sources for hostfile ~a:" (hostsfile-path)))
  (void (map (λ (x) (displayln (format  "  ~a" x))) sources)))


;; (get-sources srcs-hash) -> (listof string?)
;;   srcs-hash: hash?
;;
;; return the sources of a sources hash
(define (get-sources srcs-hash)
  (hash-map srcs-hash (λ (k v) k)))


;; (get-tags split-line) -> (listof string?)
;;   split-line: (listof string?)
;;
;; return the tags of a line, that is, the values
;; following the entry, prefaced with #!
;; eg the tags for the entry
;;    0.0.0.0 facebook.com     #! badness terrible time-wasting
;; are '("badness" "terrible" "time-wasting")
(define (get-tags split-line)
  (if (and (> (length split-line) 2) (string=? (third split-line) "#!"))
      (rest (rest (rest split-line)))
      '()))


;; (add-entry src-hash line) -> (void)
;;   src-hash: hash?
;;   line: string?
;;
;; requires: (is-entry? line)
;; side-effect: adds entry to src-hash
(define (add-entry src-hash line)
  (define split-line (string-split line))
  (define entry (second split-line))
  (hash-set! src-hash entry (get-tags split-line)))


;; (is-entry? line) -> boolean?
;;   line: string?
;;
;; determines if the given line is a valid entry
;; that is, does not start with '#'
;; TODO?: pattern match to only match lines of form
;;        XXXX.XXXX.XXXX.XXXX <URL>
;; may not be required, have to look up valid entries in hostsfile
(define (is-entry? line)
  (and (not (string=? line ""))  (not (char=? (string-ref line 0) #\#))))


;; (populate-source los srcs-hash src-hash) -> (generate-sources (rest los) srcs-hash)
;;   los: (listof string?)
;;   srcs-hash: hash?
;;   src-hash: hash?
;;
;; note: meant to be called only from (generate-sources ...)
;; generates/populates the entries for a source
(define (populate-source los srcs-hash src-hash)
  (cond [(empty? los)
         (error (error-text "expected to read '#! end src' got EOF"))]
        [(string=? (first los) "#! end src")
         (generate-sources (rest los) srcs-hash)]
        [(is-entry? (first los))
         (add-entry src-hash (first los))
         (populate-source (rest los) srcs-hash src-hash)]
        [else (populate-source (rest los) srcs-hash src-hash)]))


;; (is-line-source? line) -> (anyof string? #f)
;;   line: string?
;;
;; determines if a line is a source declaration of the form
;;    #! begin src: <SOURCE NAME>
;; if so return <SOURCE NAME> else return #f
;; TODO: rewrite using pattern matching
(define (is-line-source? line)
  (define l (string-split line))
  (if (and (> (length l) 2)
           (string=? (first l) "#!")
           (string=? (second l) "src:"))
      (third l)
      #f))


;; (get-source line) -> string?
;;   line: string?
;;
;; requires:
;;   (is-line-source? line)
;;
;; return the source of a line assuming the line is
;; of the form defined by (is-line-source? line)
(define (get-source line)
  (third (string-split line)))


;; (generate-sources los [hf-hash (make-hash)]) -> hash?
;;   los: (listof string?)
;;   hf-hash: hash?
;;
;;  given a hostsfile represented by a list of strings
;;  return a populated hash with the keys being the sources
;;  the values being a hash with the keys being the entry/hostname
;;  the values being a list of strings representing the tags
;;  associated with the entry
;; note: mutually recursive with (populate-source los srcs-hash src-hash)
(define (generate-sources los [srcs-hash (make-hash)])
  (cond [(empty? los) srcs-hash]
        [(is-line-source? (first los))
         (define source (get-source (first los)))
         (hash-set! srcs-hash source (make-hash))
         (populate-source los srcs-hash (hash-ref srcs-hash source))]
        [else (generate-sources (rest los) srcs-hash)]))


(define (main)
  (define hostsfile (open-input-file (hostsfile-path)))
  (define hostsfile-los (pipe->los hostsfile))
  (close-input-port hostsfile)
  (define sources (generate-sources hostsfile-los))

  (if (string-empty? (new-source))
      (void) (add-source (new-source) sources))
  (if (list-sources?) (list-sources sources) (void))
  (if (string-empty? (source-to-list))
      (void) (list-entries (source-to-list) sources)))
  ;(define hostsfile-out (open-output-file (hostsfile-out-path) #:exists 'append))
  ;(write-hostsfile (new-source) sources hostsfile-out)
  ;(close-output-port hostsfile-out))



;; define program parameters

;; hostsfile-path: the path of the hostsfile to be used as input
;;                 to the program
;;
;; modified with `-f` flag
(define hostsfile-path (make-parameter "/etc/hosts"))


;; hostsfile-out-path: the path of the hostsfile to be used as
;;                     output for the program
;;
;; modified with `-o` flag
(define hostsfile-out-path (make-parameter (hostsfile-path)))

;; new-source: a URL or file-path of a hostsfile to be added
;;             as a source to the hosts file specified by (hostsfile-out-path)
;;
;; specified with `-a` flag
(define new-source (make-parameter ""))

;; logging: enable logging or verbose output
;;
;; enabled with `-v` flag
(define logging (make-parameter #t))

;; list-sources?: tell program to list all the sources found
;;                in the hosts file specified by (hostsfile-path)
(define list-sources? (make-parameter #f))

;; source-to-list: tell program to list the entries for
;;                 a specified source
;;
;; specified with `-e <SOURCE>` flag
(define source-to-list (make-parameter ""))

;; define commandline flags and options for program
(define cmd
  (command-line
   #:program "hostblocker"
   #:once-each
   [("-a" "--add") source
    "add a local or remote source: <source>"
    (new-source source)]
   [("-l" "--list")
    "list known sources in the hostfile specified"
    (list-sources? #t)]
   [("-e" "--entries") source
    "list entries of a source: <source>"
    (source-to-list source)]
   [("-f" "--file") filename
    "specify hosts file: <filename>"
    (hostsfile-path filename)]
   [("-v" "--verbose")
    "display logging info"
    (logging #t)]
   [("-o" "--out") filename
    "specify output hosts file: <filename>"
    (hostsfile-out-path filename)]))
(main)
