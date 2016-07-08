#lang racket

(provide (all-defined-out)) ; temporary



(define-struct entries (sources hosts))

(define-struct hostsfile (before entries after))

(define (make-empty-hf)
  (make-hostsfile
   '() (make-entries
        (make-immutable-hash)
        (make-immutable-hash)) '()))


(define (start-cookie? line)
  (string=? line "#! HOSTBLOCKER START"))


(define (end-cookie? line)
  (string=? line "#! HOSTBLOCKER END"))

(define (start-sources? line)
  (string=? line "#! start srcs"))

(define (end-sources? line)
  (string=? line "#! end srcs"))


(define (parse-before in)
  (define line (read-line in))
  (cond [(eof-object? line) empty]
        [(start-cookie? line) empty]
        [else (cons line (parse-before in))]))


(define (get-data line)
  (define split (string-split line))
  (values (second split) (rest (rest split))))


(define (parse-line line entries)
  (define-values (host tags) (get-data line))
  (hash-set entries host tags))


(define (parse-source sources line)
  (define source (second (string-split line)))
  (hash-set sources source empty))

(define (parse-sources in [sources (make-immutable-hash)])
  (define line (read-line in))
  (cond [(eof-object? line)
         (error "EOF when END COOKIE expected")]
        [(start-sources? line) (parse-sources in sources)]
        [(end-sources? line) sources]
        [else (parse-sources in (parse-source sources line))]))


(define (parse-host hosts line)
  (define split (string-split line))
  (define host (second split))
  (define tags (rest (rest (rest split))))
  (hash-set hosts host tags))


(define (parse-hosts in [hosts (make-immutable-hash)])
  (define line (read-line in))
  (cond [(eof-object? line)
         (error "EOF when END COOKIE expected")]
        [(end-cookie? line) hosts]
        [else (parse-hosts in (parse-host hosts line))]))


(define (parse-entries in [entries (make-empty-hf)])
  (define sources (parse-sources in))
  (define hosts (parse-hosts in))
  (make-entries sources hosts))


(define (parse-after in)
  (define line (read-line in))
  (cond [(eof-object? line) empty]
        [(start-cookie? line) empty]
        [else (cons line (parse-after in))]))


(define (hf-parse in)
  (define before (parse-before in))
  (define entries (parse-entries in))
  (define after (parse-after in))
  (make-hostsfile before entries after))


(define (hf-list-sources hf out)
  (define entries (hostsfile-entries hf))
  (void (hash-map entries (λ (k v) (fprintf out (format "  ~a~n" k))))))


(define (get-line host tags)
  (string-append "0.0.0.0 " host " #! " (string-join tags " ")))

(define (hf-print hf out)
  (define before (hostsfile-before hf))
  (define entries (hostsfile-entries hf))
  (define sources (entries-sources entries))
  (define hosts (entries-hosts entries))
  (define after (hostsfile-after hf))
  (map (λ (x) (displayln x out)) before)
  (displayln "#! HOSTBLOCKER START")
  (displayln "#! start srcs")
  (hash-map sources (λ (k v) (displayln (string-append "#! " k) out)))
  (displayln "#! end srcs")
  (hash-map hosts (λ (k v) (displayln (get-line k v) out)))
  (map (λ (x) (displayln x out)) after)
  (void))
