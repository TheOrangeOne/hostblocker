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
;;   size: non-negative-integer?
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
(define-struct hostsfile (lines hosts tags sources size))

;; TODO: definition
(define-struct hostsfile-entry (tags pos) #:transparent)


(define default-hosts "/etc/hosts")


(define (is-NULL? line)
  (define NULL-LINE "#! NULL")
  (equal? line NULL-LINE))


;; (make-empty-hostsfile) -> hostsfile?
;; create an empty hostsfile
(define (make-empty-hostsfile)
  (make-hostsfile
   (make-gvector)
   (make-immutable-hash) (make-immutable-hash) (make-immutable-hash) 0))


;; (hostsfile-values hf) -> gvector? hash? hash?
;;   hf: hostsfile?
;;
;; produce the values for a hostsfile
(define (hostsfile-values hf)
  (values (hostsfile-lines   hf)
          (hostsfile-hosts   hf)
          (hostsfile-tags    hf)
          (hostsfile-sources hf)
          (hostsfile-size    hf)))


;; (hostsfile-remove-line hf line-num) -> hostsfile?
;;   hf: hostsfile?
;;   line-num: non-negative-integer?
;;
;; "removes" a line from lines by setting it to the NULL-LINE
(define (hostsfile-remove-line hf line-num)
  (define lines (hostsfile-lines hf))
  (gvector-set! lines line-num "#! NULL")
  hf)


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


;; (hostsfile-remove-host hf host) -> hostsfile?
;;   hf: hostsfile?
;;   host: string?
(define (hostsfile-remove-host hf host [hf-path default-hosts])
  (define-values (lines hosts tags srcs size) (hostsfile-values hf))
  (make-hostsfile
   lines (hash-remove (hostsfile-hosts hf) host) tags srcs size))


;; (hostsfile-tag-hosts hf tag hf-path) -> (listof string?)
;;   hf: hostsfile?
;;   tag: string?
;;   hf-path: string? = default-hosts
(define (hostsfile-tag-hosts hf tag [hf-path default-hosts])
  (define errtxt
    (error-text
     (format "tag '~a' not used by any entries in ~a" tag hf-path)))
  (hash-ref (hostsfile-tags hf) tag (λ () (error errtxt))))


;; (hostsfile-get-tags hf) -> hash?
;;   hf: hostsfile?
(define (hostsfile-get-tags hf)
  (hostsfile-tags hf))


;; (hostsfile-remove-tag hf tag) -> hostsfile?
;;   hf: hostsfile?
;;   tag: string?
(define (hostsfile-remove-tag hf tag [hf-path default-hosts])
  (define-values (lines hosts tags srcs size) (hostsfile-values hf))
  (make-hostsfile
   lines hosts (hash-remove (hostsfile-tags hf) tag) srcs size))


;; (hostsfile-get-tags-los hf) -> (listof string?)
;;   hf: hostsfile?
(define (hostsfile-get-tags-los hf)
  (define tags (hostsfile-get-tags hf))
  (hash-map tags (λ (k v) k)))


;; (hostsfile-get-hosts src hf hf-path) -> hash?
;;   hf: hostsfile?
;;   hf-path: string?
(define (hostsfile-get-hosts hf [hf-path "/etc/hosts"])
   (hostsfile-hosts hf))


;; (hostsfile-get-hosts-los src hf hf-path) -> (listof string?)
;;   src: string?
;;   hf: hostsfile?
;;   hf-path: string?
(define (hostsfile-get-hosts-los hf [hf-path "/etc/hosts"])
  (define hosts (hostsfile-get-hosts hf))
  (hash-map hosts (λ (k v) k)))


;; (fetch-hostsfile url) -> input-port?
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


(define (get-host-tags line)
  (if (string-contains? line "#!")
      (string-split (second (regexp-split "\\#!" line)))
      '()))



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
(define tag-matcher (regexp "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+.*\\#!.*"))
(define matcher (regexp "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+.*"))
(define (hostsfile-is-proper-entry? line)
  (regexp-match? tag-matcher line))

(define (hostsfile-is-entry? line)
  (regexp-match? matcher line))


  ;(and (not (string=? line ""))  (not (char=? (string-ref line 0) #\#))))


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


;; (hostsfile-get-sources hf) -> (listof string?)
;;   hf: hostsfile?
(define (hostsfile-get-sources hf)
  (hostsfile-sources hf))


;; (hostsfile-has-source? hf src) -> boolean?
;;   hf: hostsfile?
;;   src: string?
(define (hostsfile-has-source? hf src)
  (hash-has-key? (hostsfile-sources hf) src))


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


;; (hostsfile-add-source hf src) -> hostsfile?
;;   hf: hostsfile?
;;   src: string?
(define (hostsfile-add-source hf src)
  (define-values (lines hosts tags srcs size) (hostsfile-values hf))
  (make-hostsfile lines hosts tags (hash-set srcs src "")  size))


(define (sanitize-line line [newsrc ""])
  (define host (second (string-split line)))
  (get-line host (list newsrc)))


;; (hostsfile-read-line hf line) -> hostsfile?
;;   hf: hostsfile?
;;   line: (or hash? string?)
;;   newsrc: string? = ""
;;
;; given a line from a hostsfile determine if it is a valid entry that we
;; want to keep track of
;; if `line` is a valid entry as defined by (hostsfile-is-entry?) we return
;; a new hostsfile with the entry added to lines, hosts and tags
;; else we just add the line to lines
(define (hostsfile-read-line hf line [newsrc ""])
  (define-values (lines hosts tags srcs size) (hostsfile-values hf))
  (cond [(hostsfile-is-proper-entry? line)
         (hostsfile-lines-add lines line)
         (define host (get-host line))
         (define host-tags (get-host-tags line))
         (make-hostsfile
          lines
          (hostsfile-hosts-add hosts host host-tags size)
          (hostsfile-tags-add tags host host-tags)
          srcs
          (add1 size))]
        [(hostsfile-is-entry? line)
         (define sline (sanitize-line line newsrc))
         (hostsfile-lines-add lines sline)
         (define host (get-host sline))
         (define host-tags (get-host-tags sline))
         (make-hostsfile
          lines
          (hostsfile-hosts-add hosts host host-tags size)
          (hostsfile-tags-add tags host host-tags)
          srcs
          (add1 size))]
        [else
         (hostsfile-lines-add lines line)
         (make-hostsfile lines hosts tags srcs (add1 size))]))


;; (hostsfile-read-sources hf in) -> hostsfile?
;;   hf: hostsfile?
;;   in: input-port?
;;
;; assuming (is-source-start?) was read, read until (is-source-end?)
;; adding each line in between as a source
(define (hostsfile-read-sources hf in)
  (define line (read-line in))
  (cond [(eof-object? line)
         (error (error-text "expected close src"))]
        [(is-source-end? line) hf]
        [else
         (define source (second (string-split line)))
         (hostsfile-read-sources (hostsfile-add-source hf source) in)]))


;; (hostsfile-parse in hf newsrc) -> hostsfile?
;;   in: input-port?
;;   hf: hostsfile?
;;
;; given an input port produce a hostfile
(define (hostsfile-parse in [hf (make-empty-hostsfile)] [newsrc ""])
  (define line (read-line in))
  (cond
    [(eof-object? line) hf]
    [(is-source-start? line)
     (hostsfile-parse in (hostsfile-read-sources hf in) newsrc)]
    [else
     (hostsfile-parse
      in (hostsfile-read-line hf line newsrc) newsrc)]))


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
  (format "~a#!~a~a" fmt (if (empty? v) "" " ") (string-join v)))


;; (hostsfile-print-entry out entry) -> void?
;;   out: output-port?
;;   entry: (or hostsfile-source? string?)
;;
;; output a hostsfile entry to output-port `out`
(define (hostsfile-print-entry out line)
  (if (is-NULL? line)
      (void)
      (fprintf out (format "~a~n" line))))


;; (hostsfile-write hf out) -> void?
;;   hf: hostsfile?
;;   out: output-port?
;;
;; write out hostsfile to `out`
(define (hostsfile-write hf out)
  (define lines (gvector->list (hostsfile-lines hf)))
  (define sources (hostsfile-sources hf))
  (fprintf out "#! hostblocker srcs:~n")
  (hash-map sources (λ (k v) (fprintf out (format "#!  ~a~n" k))))
  (fprintf out "#! end srcs~n")
  (map (curry hostsfile-print-entry out) lines))


;; (hostsfile-add-new hf newsrc) -> hostsfile?
;;   hf: hostsfile?
;;   newsrc: string?
;;
;; add a new source to a given hostsfile, that is, create an entry in the
;; sources hash for `newsrc` and add it's entries to lines
(define (hostsfile-add-new hf newsrc)
  (define in (get-new-source newsrc))
  (if (hostsfile-has-source? hf newsrc)
      (error (error-text (format "source '~a' already exists" newsrc)))
      (hostsfile-parse in (hostsfile-add-source hf newsrc) newsrc)))


;; (hostsfile-list-hosts src hf hf-path) -> (void)
;;   src: string?
;;   hf-path: string? = default-hosts
;;   hf-path: string?
;;   out: output-port?
;;
;; side-effects:
;;   output the hosts in the given hostsfile
(define (hostsfile-list-hosts src hf hf-path out)
  (define entries (hostsfile-get-hosts-los src hf [hf-path default-hosts]))
  (displayln (format "hosts in ~a:" hf-path))
  (void (map (λ (x) (fprintf out (format  "  ~a" x))) entries)))


;; (hostsfile-list-sources srcs-hash hf-path out) -> hostsfile?
;;   srcs-hash: hash?
;;   hf-path: string? = default-hosts
;;   out: output-port?
;;
;; side-effects:
;;   print the sources of the given sources hash to `out`
(define (hostsfile-list-sources hf out [hf-path default-hosts])
  (define sources (hostsfile-get-sources hf))
  (fprintf out (format "sources for hostfile ~a:~n" hf-path))
  (void (hash-map sources (λ (k v) (fprintf out (format  "  ~a~n" k)))))
  hf)


;; (hostsfile-list-tags hf out) -> hostsfile?
;;   hf: hostsfile?
;;   out: output-port?
;;
;; print all the tags given a hostsfile to `out`
(define (hostsfile-list-tags hf out [hf-path default-hosts])
  (define tags (hostsfile-get-tags hf))
  (fprintf out (format "tags for hostfile ~a:~n" hf-path))
  (void (hash-map tags (λ (k v) (fprintf out (format  "  ~a~n" k)))))
  hf)


(define (remove-host hf host)
  (define lines (hostsfile-lines hf))
  (define hosts (hostsfile-hosts hf))
  (define line-num (hostsfile-entry-pos (hash-ref hosts host)))
  (define hf-rm-line (hostsfile-remove-line hf line-num))
  (hostsfile-remove-host hf-rm-line host))


(define (remove-hosts hf hosts)
  (cond [(empty? hosts) hf]
        [else
         (remove-hosts (remove-host hf (first hosts)) (rest hosts))]))


;; (hostsfile-remove-source hf src) -> hostsfile?
;;   hf: hostsfile?
;;   src: string?
;;   hf-path: string? = default-hosts
;;
;; removes a source from a hostsfile
(define (hostsfile-remove-source hf src [hf-path default-hosts])
  (define errtxt
    (error-text
     (format "source '~a' not found in ~a -- did you mean to specify a tag?"
             src hf-path)))
  (if (hostsfile-has-source? hf src) (void) (error errtxt))
  (define tag-hosts (hostsfile-tag-hosts hf src))
  (define hosts (hostsfile-get-hosts hf))
  (define hf-rm-hosts (remove-hosts hf tag-hosts))
  (define hf-rm-tag (hostsfile-remove-tag hf src))
  hf-rm-tag)


;; (hostsfile-remove-by-tag hf tag) -> hostsfile?
;;   hf: hostsfile?
;;   tag: string?
;;   hf-path: string? = default-hosts
;;
;; removes all hosts with tag `tag`
(define (hostsfile-remove-by-tag hf tag [hf-path default-hosts])
  (define errtxt
    (error-text
     (format "to delete a source use the -r flag")))
  (if (hostsfile-has-source? hf tag) (error errtxt) (void))
  (define tag-hosts (hostsfile-tag-hosts hf tag))
  (define hosts (hostsfile-get-hosts hf))
  (define hf-rm-hosts (remove-hosts hf tag-hosts))
  (define hf-rm-tag (hostsfile-remove-tag hf tag))
  hf-rm-tag)
