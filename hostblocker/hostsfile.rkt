#lang racket

(require
 net/http-client
 net/head
 net/url
 "util.rkt"
)

(provide (all-defined-out)) ; temporary


;; START-COOKIE designates the beginning of the block that
;; `hostblocker` reads
(define START-COOKIE "#! HOSTBLOCKER START")

(define (start-cookie? line)
  (string=? line START-COOKIE))


;; END-COOKIE designates the end of the block that `hostblocker`
;; reads
(define END-COOKIE "#! HOSTBLOCKER END")

(define (end-cookie? line)
  (string=? line END-COOKIE))


;; START-SRCS designates the start of the sources
(define START-SRCS "#! start srcs")

(define (start-sources? line)
  (string=? line START-SRCS))


;; END-SRCS designates the end of the sources
(define END-SRCS "#! end srcs")

(define (end-sources? line)
  (string=? line END-SRCS))


;; entries represents the various sources and hosts contained between
;; `START-COOKIE` and `END-COOKIE`
;;
;; sources: hash?
;;   the sources located between `START-SRCS` and `END-SRCS`, the keys
;;   being the name of the source and the value being the location
;;
;; hosts: hash?
;;   the hosts contained in the hostblocker cookies, the keys being
;;   domain/host the value being a list of strings, the tags for a
;;   host
(struct entries (sources hosts))

(define (make-empty-entries)
  (entries (make-immutable-hash) (make-immutable-hash)))


;; hostsfile represents hostblocker's interpretation of the
;; user's hostsfile
;;
;; before: (listof string?)
;;   represents all the lines in the hostsfile before
;;   `START-COOKIE`
;;
;; entries: entries?
;;   represent the lines between `START-COOKIE` and `END-COOKIE`
;;
;; after: (listof string?)
;;   all the lines in the hostsfile after `END-COOKIE`
(struct hostsfile (before entries after))

(define (make-empty-hf)
  (hostsfile '() (make-empty-entries) '()))


(define (hf-set-hosts hf newhosts)
  (define myentries (hostsfile-entries hf))
  (struct-copy
   hostsfile hf
   [entries
    (struct-copy
     entries myentries
     [hosts newhosts])]))


(define host-matcher (regexp "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+ .*"))
(define (hf-is-host? line)
  (regexp-match? host-matcher line))


(define tag-matcher (regexp "^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+.*\\#!.*"))
(define (hf-is-host-w-tags? line)
  (regexp-match? tag-matcher line))


(define (parse-init in)
  (define line (read-line in))
  (cond [(eof-object? line) empty]
        [(start-cookie? line)
         (error "file appears to be initialized already!")]
        [(or (start-sources? line)
             (end-sources? line)
             (end-cookie? line))
         (error "file appears to be hostblocker-corrupt!")]
        [else (cons line (parse-init in))]))


(define (parse-before in)
  (define line (read-line in))
  (cond [(eof-object? line)
         (error
          (string-append
           "file does not appear to be initialized!\n"
           "try running:\n"
           "  $ hostblocker --init <filename>"))]
        [(start-cookie? line) empty]
        [else (cons line (parse-before in))]))


(define (get-host-tags line)
  (define split (string-split line))
  (values (second split) (rest (rest split))))


(define (get-tags split-line)
  (rest (rest (rest split-line))))


(define (get-tags-unknown src split-line)
  (cons src (rest (rest (rest split-line)))))


(define (parse-line line ents)
  (define-values (host tags) (get-host-tags line))
  (hash-set ents host tags))


(define (parse-source sources line)
  (define split (string-split line))
  (define name (second split))
  (define source (third split))
  (hash-set sources name source))


(define (parse-sources in [sources (make-immutable-hash)])
  (define line (read-line in))
  (cond [(eof-object? line)
         (error "EOF when END COOKIE expected")]
        [(start-sources? line) (parse-sources in sources)]
        [(end-sources? line) sources]
        [else (parse-sources in (parse-source sources line))]))


(define (parse-host hosts line)
  (cond
    [(hf-is-host-w-tags? line)
     (define split (string-split line))
     (define host (second split))
     (define tags (get-tags split))
     (hash-set hosts host tags)]
    [else (error "invalid host")]))


(define (parse-hosts in [hosts (make-immutable-hash)])
  (define line (read-line in))
  (cond [(eof-object? line)
         (error "EOF when END COOKIE expected")]
        [(end-cookie? line) hosts]
        [else (parse-hosts in (parse-host hosts line))]))


(define (parse-entries in [ents (make-empty-hf)])
  (define sources (parse-sources in))
  (define hosts (parse-hosts in))
  (entries sources hosts))


(define (parse-after in)
  (define line (read-line in))
  (cond [(eof-object? line) empty]
        [(start-cookie? line) empty]
        [else (cons line (parse-after in))]))


(define (parse-unknown-host hosts line [src ""])
  (define host-with-tags? (hf-is-host-w-tags? line))
  (define host? (hf-is-host? line))
  (cond
    [(or host? host-with-tags?)
     (define split (string-split line))
     (define host (second split))
     (define tags (if host-with-tags?
                      (get-tags-unknown src split)
                      (list src)))
     (define addedhosts (hash-set hosts host tags))

     ; check for duplicate
     ; if so prompt user to replace
     ; else add host
     (if (and (hash-has-key? hosts host)
              (not (equal? src (first tags))))
         (confirmation
          (string-append
           "host '"host
           "' already included from source '"
           (first tags)"', replace host?")
          addedhosts
          hosts)
         addedhosts)]
    [else hosts]))


(define (parse-unknown-hosts in [hosts (make-immutable-hash)] [src ""])
  (define line (read-line in))
  (cond [(eof-object? line) hosts]
        [else (parse-unknown-hosts in (parse-unknown-host hosts line src) src)]))


(define (parse in)
  (define before (parse-before in))
  (define entries (parse-entries in))
  (define after (parse-after in))
  (hostsfile before entries after))


(define (remote-hostsfile url)
  (displayln (format "fetching remote source ~a..." url))
  (with-handlers
    ([(λ (x) #t)
      (λ (x) (error (format "could not connect to ~a" url)))])
    (define-values
      (status header resp)
      (http-sendrecv/url (string->url url) #:method "GET"))
    (displayln (format "fetch successful"))
    resp))


(define (local-hostsfile path)
  (with-handlers
    ([(λ (x) #t)
      (λ (x)
        (error (format "could not open or find file '~a'" path)))])
    (open-input-file path)))


(define (get-src src-file)
  (if (regexp-match? url-regex src-file)
      (remote-hostsfile src-file)
      (local-hostsfile src-file)))


(define (remove-keys hash lst)
  (if (empty? lst)
      hash
      (remove-keys (hash-remove hash (first lst)) (rest lst))))


(define (remove-hosts-with-source hosts src)
  (define keys
    (filter
     (λ (x) (not (string=? x "")))
     (hash-map hosts (λ (k v) (if (string=? (first v) src) k "")))))
  (remove-keys hosts keys))


(define (get-line host tags)
  (string-append "0.0.0.0 " host " #! " (string-join tags " ")))


(define (hf-add-source hf src name)
  (define myentries (hostsfile-entries hf))
  (define srcs (entries-sources myentries))
  (if (hash-has-key? srcs name)
      (error "source already exists with that name!")
      (void))
  (struct-copy
   hostsfile hf
   [entries
    (struct-copy
     entries myentries
     [sources (hash-set srcs name src)])]))


(define (hf-remove-source hf name)
  (define myentries (hostsfile-entries hf))
  (define srcs (entries-sources myentries))
  (if (hash-has-key? srcs name)
      (void)
      (error "source not found!"))
  (struct-copy
   hostsfile hf
   [entries
    (struct-copy
     entries myentries
     [sources (hash-remove srcs name)])]))


(define (hf-write hf out)
  (define before (hostsfile-before hf))
  (define entries (hostsfile-entries hf))
  (define sources (entries-sources entries))
  (define hosts (entries-hosts entries))
  (define after (hostsfile-after hf))
  (map (λ (x) (displayln x out)) before)
  (displayln START-COOKIE out)
  (displayln START-SRCS out)
  (hash-map sources (λ (k v) (displayln (string-append "#! "k" "v) out)))
  (displayln END-SRCS out)
  (hash-map hosts (λ (k v) (displayln (get-line k v) out)))
  (displayln END-COOKIE out)
  (map (λ (x) (displayln x out)) after)
  (void))


(define (hf-update-source hosts src)
  (define name (first src))
  (define source (second src))
  (define in (get-src source))
  (displayln (string-append "updating source '"source"'..."))
  (define newhosts (parse-unknown-hosts in hosts name))
  (displayln (string-append "source updated successfully"))
  newhosts)


(define (hf-update-sources hosts srcs)
  (cond
    [(empty? srcs) hosts]
    [else
     (hf-update-sources (hf-update-source hosts (first srcs)) (rest srcs))]))
