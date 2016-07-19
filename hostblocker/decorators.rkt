#lang racket

(require "hostsfile.rkt")

(provide (all-defined-out))


;; this module defines functions (decorators) which correspond to different
;; commands that act upon a `hostsfile` and produce a new `hostsfile`


(define (hf-init in)
  (hostsfile (parse-init in) (make-empty-entries) '()))

(define (init-dec)
  (λ (in) (hf-init in)))


(define (add-source hf src name)
  (define in (get-src src))
  (define hosts (entries-hosts (hostsfile-entries hf)))
  (define newhosts (parse-unknown-hosts in hosts name))
  (hf-set-hosts (hf-add-source hf src name) newhosts))


(define (remove-source hf src)
  (define hosts (entries-hosts (hostsfile-entries hf)))
  (hf-set-hosts (hf-remove-source hf src) (remove-hosts-with-source hosts src)))


(define (list-sources hf out)
  (define entries (hostsfile-entries hf))
  (define sources (entries-sources entries))
  (fprintf out "sources:~n")
  ;; print the sources in sorted order
  (map (λ (x) (fprintf out x))
       (sort (hash-map sources (λ (k v) (format "  ~a~n" k))) string<=?))
  hf)


(define (update-sources hf)
  (define srcs (entries-sources (hostsfile-entries hf)))
  (define srcs-kv (hash-map srcs (λ (k v) (list k v))))
  (define hosts (hf-update-sources (make-immutable-hash) srcs-kv))
  (hf-set-hosts hf hosts))


(define (defaults hf)
  (add-source
   hf
   "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
   "ads+malware"))
