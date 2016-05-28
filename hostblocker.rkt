#lang racket

(require
 "lib.rkt"
 racket/cmdline
 net/http-client
 net/head
 net/url
 data/gvector)

(provide (all-defined-out))


;; prevent stacktrace on error messages -- comment-out when developing!
(error-print-context-length 0)


;; define a hostsfile as the following:
;;   entries: (gvector (anyof string? hash?))
;;   srcs: hash?
;;   tags: hash?
;;
;; we wish to maintain the ordering of entries and source blocks within
;; the hostsfile in case there are manually added entries which the user
;; would not like us to mess with
;;
;; string? elements in `entries` represent raw lines from the hostsfile
;; not contained within #! source blocks
;;
;; hash? elements in `entries` represent sources
;;
;; `srcs` is a hash containing all the sources as keys with the values also
;; being a hash with keys being all of the entries for a source and the
;; value for each entry being a list of strings for the tags of the entry
;;
;; `tags` is a hash containing all the entries' tags the keys being the tag
;; (string?) the values being a list of string, the entries possessing
;; the tag
(define-struct hostsfile (entries sources tags orig))

;;  name: string?
;;  entries: hash?
;; TODO: finish definition
(define-struct hostsfile-source (name entries))

;; (make-empty-hostsfile) -> hostsfile?
;; create an empty hostsfile
(define (make-empty-hostsfile)
  (make-hostsfile (make-gvector) (make-hash) (make-hash) '()))


;; (hostsfile-values hf) -> gvector? hash? hash? list?
;;   hf: hostsfile?
;;
;; produce the values for a hostsfile
(define (hostsfile-values hf)
  (values (hostsfile-entries hf)
          (hostsfile-sources hf)
          (hostsfile-tags hf)
          (hostsfile-orig hf)))


;; (add-tag-entry src tags-hash tag) -> void?
;;   src: string?
;;   tags-hash: hash?
;;   tag: string?
;;
;; add a host
(define (add-tag-entry tags-hash host tag)
  (cond [(hash-has-key? tags-hash tag)
         (define vals (hash-ref tags-hash tag))
         (hash-set! tags-hash tag (cons host vals))]
        [else
         (hash-set! tags-hash tag (list host))]))


;; (fetch-hostsfile url) -> (input-pipe)
;;   url: string?
;;
;; GET hostsfile located at url
(define (get-remote-hostsfile url)
  (with-handlers
    ([(λ (x) #t)
      (λ (x) (error (error-text (format "could not connect to ~a" url))))])
    (define-values
      (status header resp)
      (http-sendrecv/url (string->url url) #:method "GET"))
    resp))


;; TODO: document or clean up
(define (get-local-hostsfile path)
  (with-handlers
    ([(λ (x) #t)
      (λ (x)
        (error
         (error-text (format "could not open or find file '~a'" path))))])
    (open-input-file path)))


;; TODO: document or clean up
(define (get-new-source src-file)
  (if (regexp-match? url-regex src-file)
      (get-remote-hostsfile src-file)
      (get-local-hostsfile src-file)))


;; TODO: document or clean up
;(define (generate-entries newsrc-pipe sources [entries (make-hash)])
;  (define line (read-line newsrc-pipe))
;  (cond [(eof-object? line) entries]
;        [(is-entry? line)
;         (add-source-entry entries line)
;         (generate-entries newsrc-pipe sources entries)]
;        [else (generate-entries newsrc-pipe sources entries)]))


(define (hostsfile-add-new-source hf newsrc)
  (define sources (hostsfile-sources hf))
  (define entries (hostsfile-entries hf))
  (if (hash-has-key? sources newsrc)
      (error (error-text (format "source '~a' already exists" newsrc)))
      (void))
  (hash-set! sources newsrc (make-hash))
  (gvector-add! entries "")
  (define newsrc-pipe (get-new-source newsrc))
  (hostsfile-add-source-in hf newsrc newsrc-pipe #f))


;; (hostsfile-add-entry hf val) -> hostsfile?
;;   hf: hostsfile?
;;   val: (or hash? string?)
;;
;; produce a hostsfile with val added to entries and if val is a string add
;; val to orig
(define (hostsfile-add-entry hf val)
  (define-values (ents srcs tags orig) (hostsfile-values hf))
  (gvector-add! ents val)
  (make-hostsfile
   ents srcs tags (if (string? val) (cons val orig) orig)))


;; (hostsfile-add-source-entry hf src entry) -> hostsfile?
;;   hf: hostsfile?
;;   src: string?
;;   entry: string?
;;
;; given a hostsfile, source and entry produce a hostsfile
(define (hostsfile-add-source-entry hf src entry)
  (define-values (ents srcs tags orig) (hostsfile-values hf))
  (define src-hash (hash-ref srcs src))
  (add-source-entry src-hash entry)
  (define entry-tags (get-tags entry))
  (map (curry (curry add-tag-entry tags) (get-host entry)) entry-tags)
  (make-hostsfile ents srcs tags (cons entry orig)))


;; (hostsfile-is-entry? line) -> boolean?
;;   line: string?
;;
;; determines if the given line is a valid entry
;; that is, does not start with '#'
;; TODO: pattern match to only match lines of form
;;        XXXX.XXXX.XXXX.XXXX <URL>
;; may not be required, have to look up valid entries in hostsfile
(define (hostsfile-is-entry? line)
  (and (not (string=? line ""))  (not (char=? (string-ref line 0) #\#))))


;; (hostsfile-add-source-in hf src in err) -> hostsfile?
;;   hf: hostsfile?
;;   src: string?
;;   in: input-port?
;;   err: boolean?
;;
;; assuming the "#! src: `src`" line has been read, read input from `in`
;; populating `src` with entries until the line "#! end src" is read
;; if err? throw an error on reading eof else return the hostsfile
(define (hostsfile-add-source-in hf src in [err-on-eof #t])
  (define line (read-line in))
  (cond
    [(eof-object? line)
     (if err-on-eof
         (error (error-text "expected to read '#! end src' got EOF"))
         (hostsfile-add-entry
          hf (make-hostsfile-source
              src (hash-ref (hostsfile-sources hf) src))))]
    [(string=? line "#! end src")
     ;; at this point we just want to add the new sources
     ;; hash to the entries list
     (define source
       (make-hostsfile-source src (hash-ref (hostsfile-sources hf) src)))
      (hostsfile-add-entry hf source)]
    [(hostsfile-is-entry? line)
     ;; we want to add the line to the sources hash
     ;; as well as add any tags the entry has
     (hostsfile-add-source-in
      (hostsfile-add-source-entry hf src line) src in err-on-eof)]
    [else
     ;; ignore all other entries contained within src blocks
     (hostsfile-add-source-in hf src in err-on-eof)]))


;; (parse in hf) -> hostsfile?
;;   in: input-port?
;;   hf: hostsfile?
;;
;; given an input port produce a hostfile
(define (hostsfile-parse in [hf (make-empty-hostsfile)])
  (define line (read-line in))
  (cond
    [(eof-object? line) hf]
    [(is-line-source? line)
     (define src (get-source line))
     (hash-set! (hostsfile-sources hf) src (make-hash))
     (hostsfile-parse in (hostsfile-add-source-in hf src in))]
    [else
     (hostsfile-parse in (hostsfile-add-entry hf line))]))


;; (get-line k v) -> string?
;;   k: string?
;;   v: (listof string?)
;;
;; returns a line/entry for hostsfile given the
;; host `k` and the tags for the host `v`
;; eg:
;; (get-line "facebook.com" '("crap" "bad")) -> "0.0.0.0 facebook.com #! crap bad"
(define (get-line k v)
  (define host (format "0.0.0.0 ~a" k))
  (define len (string-length host))
  (define fmt (~a host #:min-width 90))
  (format "~a#!~a~a~n" fmt (if (empty? v) "" " ") (string-join v)))


;; (hostsfile-print-entry out entry) -> void?
;;   out: output-port?
;;   entry: (or hostsfile-source? string?)
;;
;; output a hostsfile entry to output-port `out`
(define (hostsfile-print-entry out entry)
  (cond
    [(hostsfile-source? entry)
     (define src-name (hostsfile-source-name entry))
     (define src-hash (hostsfile-source-entries entry))
     (fprintf out (format "#! src: ~a~n" src-name))
     (hash-map src-hash (λ (k v) (fprintf out (get-line k v))))
     (fprintf out (format "#! end src~n"))]
    [else
     (fprintf out (format "~a~n" entry))]))


;; (hostsfile-write hf out) -> void?
;;   hf: hostsfile?
;;   out: output-port?
;;
;; write out hostsfile to `out`
(define (hostsfile-write hf out)
  (define entries (gvector->list (hostsfile-entries hf)))
  (map (curry hostsfile-print-entry out) entries))


;; (hostsfile-list-entries src hf) -> (void)
;;   src: string?
;;   hf: hostsfile?
;;
;; requires:
;;   parameters: (hostsfile-path)
;; side-effects:
;;   output the entries for a given source in the given hostsfile
(define (hostsfile-list-entries src hf)
  (define entries (hostsfile-get-entries src hf))
  (displayln (format "entries for source ~a:" src))
  (void (map (λ (x) (displayln (format  "  ~a" x))) entries)))


;; (hostsfile-get-entries src hf) -> (listof string?)
;;   src: string?
;;   hf: hostsfile?
;;
;; requires:
;;   parameters: (hostsfile-path)
;;
;; return the entries for a given source `src`
(define (hostsfile-get-entries src hf)
  (define srcs-hash (hostsfile-sources hf))
  (define errtxt
    (error-text
     (format "source '~a' not found in ~a" src (hostsfile-path))))
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
(define (hostsfile-list-sources srcs-hash)
  (define sources (hostsfile-get-sources srcs-hash))
  (displayln (format "sources for hostfile ~a:" (hostsfile-path)))
  (void (map (λ (x) (displayln (format  "  ~a" x))) sources)))


;; (get-sources srcs-hash) -> (listof string?)
;;   srcs-hash: hash?
;;
;; return the sources of a sources hash
(define (hostsfile-get-sources srcs-hash)
  (hash-map srcs-hash (λ (k v) k)))


;; (log line) -> void?
;;   line: string?
;; requires:
;;   parameters: (logging)
;;
;; if logging is enabled (equal? (logging) #t) then output the given
;; string `line`
(define (log line)
  (if (logging) (displayln line) (void)))


;; (get-tags line) -> (listof string?)
;;   line: (or string? (listof string?))
;;
;; return the tags of a line, that is, the values
;; following the entry, prefaced with #!
;; eg the tags for the entry
;;    0.0.0.0 facebook.com     #! badness terrible time-wasting
;; are '("badness" "terrible" "time-wasting")
(define (get-tags line)
  (define split-line
    (if (string? line) (string-split line) line))
  (if (and (> (length split-line) 2) (string=? (third split-line) "#!"))
      (rest (rest (rest split-line)))
      '()))


;; (add-source-entry src-hash line) -> (void)
;;   src-hash: hash?
;;   line: string?
;;
;; requires: (is-entry? line)
;; side-effect: adds entry to src-hash
(define (add-source-entry src-hash line)
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


;; (populate-source los srcs-hash src-hash)
;;                               -> (generate-sources (rest los) srcs-hash)
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
         (add-source-entry src-hash (first los))
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


;; (get-host line) -> string?
;;   line: string?
;;
;; given an entry `line` return the host
;; TODO: rewrte using pattern matching
(define (get-host line)
  (second (string-split line)))


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
  (define in (open-input-file (hostsfile-path)))
  (define myhostsfile (hostsfile-parse in))
  (close-input-port in)

  (map (λ (x) (x myhostsfile)) (reverse (modifiers)))

  (cond
    [(modify?)
     (define hostsfile-out
       (open-output-file (hostsfile-out-path) #:exists 'replace))
     (hostsfile-write myhostsfile hostsfile-out)
     (close-output-port hostsfile-out)]
    [else
     ; print to stdout
     ;(hostsfile-write myhostsfile (current-output-port))
     (void)])
  (void))




;; define program parameters

;; hostsfile-path: the path of the hostsfile to be used as input to the
;;                 program
(define hostsfile-path (make-parameter "/etc/hosts"))


;; hostsfile-out-path: the path of the hostsfile to be used as output for
;;                     the program
(define hostsfile-out-path (make-parameter (hostsfile-path)))

;; modifiers: a list of functions that accept a single parameter, a
;;            hostsfile that get applied in (main)
(define modifiers (make-parameter '()))

;; modify?: tell program to write to (hostsfile-out-path)
(define modify? (make-parameter #f))

;; logging: enable logging or verbose output
(define logging (make-parameter #t))

;; list-sources?: tell program to list all the sources found in the hosts
;;                file specified by (hostsfile-path)
(define list-sources? (make-parameter #f))



;; define commandline flags and options for program
(define cmd
  (command-line
   #:program "hostblocker"
   #:once-each
   [("-f" "--file") filename
    "specify hosts file to use: <filename>"
    (hostsfile-path filename)
    (hostsfile-out-path (hostsfile-path))]

   [("-v" "--verbose")
    "display logging info"
    (logging #t)]

   [("-o" "--out") filename
    "specify output hosts file: <filename>"
    (modify? #t)
    (hostsfile-out-path filename)]

   [("-l" "--list")
    "list known sources in the hostfile specified"
    (define list-sources-dec
      (λ (x) (hostsfile-list-sources (hostsfile-sources x))))
    (modifiers
     (cons list-sources-dec (modifiers)))]

   #:multi
   [("-a" "--add") source
    "add a local or remote source: <source>"
    (modify? #t)
    (define add-source-dec
      (λ (x) (hostsfile-add-new-source x source)))
    (modifiers
     (cons add-source-dec (modifiers)))]

   [("-s" "--list-source") source
    "list entries of a source: <source>"
    (define list-entries-dec
      (λ (x) (hostsfile-list-entries source x)))
    (modifiers
     (cons list-entries-dec (modifiers)))]))
(main)
