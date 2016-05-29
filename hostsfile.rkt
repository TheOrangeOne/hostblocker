#lang racket

(require
 "lib.rkt"
 racket/cmdline
 net/http-client
 net/head
 net/url
 data/gvector)

(provide (all-defined-out))


;; define a hostsfile as the following:
;;   entries: (gvector-of string?)
;;   hosts: hash?
;;   tags: hash?
;;
;; we wish to maintain the ordering of entries within the hostsfile in case
;; there are manually added entries which the user would not like us to mess
;; with
;;
;; `lines` is a vector (gvector) of strings which is a direct representation
;; of the hostsfile
;;
;; `hosts` is a hash mapping between hosts (string?) and hostsfile-entry
;; objects, this gives us constant time lookups of hosts' tags and position
;;
;; `tags` is a hash mapping between a tag and each host that has that tag
(define-struct hostsfile (lines hosts tags sources))

;; TODO: definition
(define-struct hostsfile-entry (tags position))


(define default-hosts "/etc/hosts")

;; (make-empty-hostsfile) -> hostsfile?
;; create an empty hostsfile
(define (make-empty-hostsfile)
  (make-hostsfile
   (make-gvector) (make-immutable-hash) (make-immutable-hash) '()))


;; (hostsfile-values hf) -> gvector? hash? hash?
;;   hf: hostsfile?
;;
;; produce the values for a hostsfile
(define (hostsfile-values hf)
  (values (hostsfile-lines   hf)
          (hostsfile-hosts   hf)
          (hostsfile-tags    hf)
          (hostsfile-sources hf)))


;; (hostsfile-get-host hf host hf-path) -> hostsfile-entry?
;;   hf: hostsfile?
;;   host: string?
;;   hf-path: string?
(define (hostsfile-get-host hf host [hf-path default-hosts])
  (define errtxt
    (error-text
     (format "host '~a' not found in ~a" host hf-path)))
  (hash-ref (hostsfile-hosts hf) host (λ () (error errtxt))))


;; (hostsfile-host-tags hf host hf-path) -> (listof string?)
;;   hf: hostsfile?
;;   host: string?
;;   hf-path: string? = default-hosts
(define (hostsfile-host-tags hf host [hf-path default-hosts])
  (hostsfile-entry-tags (hostsfile-get-host hf host hf-path)))


;; (hostsfile-tag-hosts hf tag hf-path) -> (listof string?)
;;   hf: hostsfile?
;;   tag: string?
;;   hf-path: string? = default-hosts
(define (hostsfile-tag-hosts hf tag [hf-path default-hosts])
  (define errtxt
    (error-text
     (format "tag '~a' not used by any entries in ~a" tag hf-path)))
  (hash-ref (hostsfile-tags hf) tag (λ () (error errtxt))))


;; (hostsfile-get-entries-los src hf hf-path) -> (listof string?)
;;   src: string?
;;   hf: hostsfile?
;;   hf-path: string?
(define (hostsfile-get-hosts-los hf [hf-path "/etc/hosts"])
  (define hosts (hostsfile-hosts hf))
  (hash-map hosts (λ (k v) k)))


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
  (if (regexp-match? lib-url-regex src-file)
      (get-remote-hostsfile src-file)
      (get-local-hostsfile src-file)))


;; (get-host line) -> string?
;;   line: string?
;;
;; given an entry `line` return the host
;; TODO: rewrte using pattern matching
(define (get-host line)
  (second (string-split line)))


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


(define (is-source-start? line)
  (string=? line "#! hostblocker srcs:"))


(define (is-source-end? line)
  (string=? line "#! end srcs"))


;; (hostsfile-has-host? hf host) -> boolean?
;;   hf: hostsfile?
;;   host: string?
;;
;; return #t if the hostsfile has host, #f otherwise
(define (hostsfile-has-host? hf host)
  (hash-has-key? (hostsfile-hosts hf) host))


;; (hostsfile-has-tag? hf tag) -> boolean?
;;   hf: hostsfile?
;;   tag: string?
;;
(define (hostsfile-has-tag? hf tag)
  (define tags (hostsfile-tags hf))
  (hash-has-key? tags tag))


;; (hostsfile-lines-add lines line) -> (void)
;;   lines: gvector?
;;   line: string?
(define (hostsfile-lines-add lines line)
  (gvector-add! lines line))


;; (hostsfile-add-line hf line) -> (void)
;;   hf: hostsfile?
;;   line: string?
(define (hostsfile-add-line hf line)
  (hostsfile-lines-add (hostsfile-lines hf) line))


;; (hostsfile-hosts-add hosts host tags line-num) -> hash?
;;   hosts: hash?
;;   host: string?
;;   tags: (listof string?)
;;   line-num: non-negative-integer?
(define (hostsfile-hosts-add hosts host tags line-num)
  (hash-set hosts host (make-hostsfile-entry tags line-num)))


;; (hostsfile-tags-add src tags tag) -> hash?
;;   src: string?
;;   tags: hash?
;;   tag: string?
;;
;; add a host to the list of hosts that have the tag `tag`
(define (hostsfile-tags-add-host tags host tag)
  (if (hash-has-key? tags tag)
      (hash-set tags tag (cons host (hash-ref tags tag)))
      (hash-set tags tag (list host))))


;; (hostsfile-tags-add tags host lst) -> hash?
;;   tags: hash?
;;   host: string?
;;   lst: (listof string?)
;;
;; given a host and its tags add mappings between each tag and the host
;; cons-ing the host onto existing hosts if there are any
(define (hostsfile-tags-add tags host lst)
  (cond [(empty? lst) tags]
        [else
         (hostsfile-tags-add
          (hostsfile-tags-add-host tags host (first lst))
          host (rest lst))]))


;; (hostsfile-lines-add lines line) -> hostsfile?
;;   hf: hostsfile?
;;   src: string?
(define (hostsfile-add-source hf src)
  (define-values (lines hosts tags srcs) (hostsfile-values hf))
  (hostsfile-add-line hf src)
  (make-hostsfile lines hosts tags (cons src srcs)))


;; (hostsfile-read-line hf line line-num) -> hostsfile?
;;   hf: hostsfile?
;;   line: (or hash? string?)
;;   line-num: non-negative-integer?
;;
;; given a line from a hostsfile determine if it is a valid entry that we
;; want to keep track of
;; if `line` is a valid entry as defined by (hostsfile-is-entry?) we return
;; a new hostsfile with the entry added to lines, hosts and tags
;; else we just add the line to lines
(define (hostsfile-read-line hf line line-num)
  (define-values (lines hosts tags srcs) (hostsfile-values hf))
  (hostsfile-lines-add lines line)
  (cond [(hostsfile-is-entry? line)
         (define host (get-host line))
         (define host-tags (get-tags line))
         (make-hostsfile
          lines
          (hostsfile-hosts-add hosts host host-tags line-num)
          (hostsfile-tags-add tags host host-tags)
          srcs)]
        [else
         (make-hostsfile lines hosts tags srcs)]))


;; (hostsfile-get-sources hf in) -> hostsfile?
;;   hf: hostsfile?
;;   in: input-port?
;;
;; assuming (is-source-start?) was read, read until (is-source-end?)
;; adding each line in between as a source
(define (hostsfile-read-sources hf in)
  (define line (read-line in))
  (cond [(eof-object? line)
         (error (error-text "expected close src"))]
        [(is-source-end? line) (hostsfile-add-line hf line) hf]
        [else
         (hostsfile-add-source hf line)]))


;; (hostsfile-parse in hf) -> hostsfile?
;;   in: input-port?
;;   hf: hostsfile?
;;
;; given an input port produce a hostfile
(define (hostsfile-parse in [hf (make-empty-hostsfile)] [line-num 0])
  (define line (read-line in))
  (cond
    [(eof-object? line) hf]
    [(is-source-start? line)
     (hostsfile-add-line hf line)
     (hostsfile-parse in (hostsfile-read-sources hf in))]
    [else
     (hostsfile-parse
      in (hostsfile-read-line hf line line-num) (add1 line-num))]))


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
(define (hostsfile-print-entry out line)
  (fprintf out (format "~a~n" line)))


;; (hostsfile-write hf out) -> void?
;;   hf: hostsfile?
;;   out: output-port?
;;
;; write out hostsfile to `out`
(define (hostsfile-write hf out)
  (define lines (gvector->list (hostsfile-lines hf)))
  (map (curry hostsfile-print-entry out) lines))


;; (hostsfile-list-hosts src hf hf-path) -> (void)
;;   src: string?
;;   hf: hostsfile?
;;   hf-path: string?
;;
;; side-effects:
;;   output the hosts in the given hostsfile
(define (hostsfile-list-hosts src hf hf-path)
  (define entries (hostsfile-get-hosts-los src hf [hf-path "/etc/hosts"]))
  (displayln (format "hosts in ~a:" hf-path))
  (void (map (λ (x) (displayln (format  "  ~a" x))) entries)))



;; (hostsfile-get-entries hf src hf-path) -> (hash?)
;;   hf: hostsfile?
;;   src: string?
;;   hf-path: string?
;;
;; return the entries for a given source `src`
;(define (hostsfile-get-entries hf src [hf-path "/etc/hosts"])
;  (define hosts (hostsfile-hosts hf))
;  (define sources (hostsfile-sources hf))
;  (define errtxt
;    (error-text
;     (format "source '~a' not found in ~a" src hf-path)))
;  (if (in-list? sources src) (void) (error errtxt))
;  (hash-map
;   hosts (λ (k v) (if (in-list? v src) k)))
  ;(hash-ref srcs-hash src (λ () (error errtxt))))


;; (list-sources srcs-hash hf-path) -> (void)
;;   srcs-hash: hash?
;;   hf-path: string?
;;
;; side-effects:
;;   print out the sources given the sources hash
;(define (hostsfile-list-sources srcs-hash [hf-path "/etc/hosts/"])
;  (define sources (hostsfile-get-sources srcs-hash))
;  (displayln (format "sources for hostfile ~a:" hf-path))
;  (void (map (λ (x) (displayln (format  "  ~a" x))) sources)))


;; (hostsfile-get-sources srcs-hash) -> (listof string?)
;;   srcs-hash: hash?
;;
;; return the sources of a sources hash
;(define (hostsfile-get-sources srcs-hash)
;  (hash-map srcs-hash (λ (k v) k)))


;; (hostsfile-remove-source hf src) -> hostsfile?
;;   hf: hostsfile?
;;   src: string?
;;
;; TODO: make constant time by keeping track of
;;       position in entries
(define (hostsfile-remove-source hf src)
  (void))

