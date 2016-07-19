#lang racket

(require
 "hostsfile.rkt"
 "decorators.rkt"
 )

;; prevent stacktrace on error messages -- comment-out when developing!
(error-print-context-length 0)


;; (log line) -> void?
;;   line: string?
;; requires:
;;   parameters: (logging)
;;
;; if logging is enabled (equal? (logging) #t) then output the given
;; string `line`
(define (log line)
  (if (logging) (displayln line) (void)))


;; (decorate hf fns) -> hf
;;   hf: hostsfile?
;;   fns: (listof function)
;;
;; decorate and produce a hostsfile with a list of decorators
;; that accept a hostsfile `hf`
(define (decorate hf fns)
  (cond [(empty? fns) hf]
        [else (decorate ((first fns) hf) (rest fns))]))


;; (initialize) -> void?
;;
;; initialize a file with hostblocker
(define (initialize)
  (define in (open-input-file (hostsfile-path)))
  (define hf (hf-init in))
  (close-input-port in)
  (define out (open-output-file (hostsfile-out-path) #:exists 'replace))
  (hf-write hf out)
  (close-output-port out)
  (void))


(define (main)
  (define in (open-input-file (hostsfile-path)))
  (define hf (parse in))
  (close-input-port in)

  (define decd-hf (decorate hf (reverse (modifiers))))

  (cond
    [(modify?)
     (define out (open-output-file (hostsfile-out-path) #:exists 'replace))
     (hf-write decd-hf out)
     (close-output-port out)]
    [else (void)])
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

;; initialize?: tell program to initialize (hostfile-path)
(define initialize? (make-parameter #f))

;; logging: enable logging or verbose output
(define logging (make-parameter #t))


(define (add-modifier mod mods)
  (cons mod mods))

;; define commandline flags and options for program
(define cmd
  (command-line
   #:program "hostblocker"
   #:once-each
   [("-i" "--init") filename
    "intialize <filename> with hostblocker"
    (hostsfile-path filename)
    (hostsfile-out-path filename)
    (initialize? true)]
   [("-d" "--defaults")
    "add the entries from https://github.com/StevenBlack/hosts"
    (modify? #t)
    (define defaults-dec (λ (x) (defaults x)))
    (modifiers (add-modifier defaults-dec (modifiers)))]
   [("-f" "--file") filename
    "specify hosts file to use: <filename>"
    (hostsfile-path filename)
    (hostsfile-out-path (hostsfile-path))]
   [("-v" "--verbose")
    "display logging info"
    (logging #t)]
   [("-u" "--update")
    "update sources in the hostsfile"
    (modify? #t)
    (define update-sources-dec (λ (x) (update-sources x)))
    (modifiers (add-modifier update-sources-dec (modifiers)))]
   [("-o" "--out") filename
    "specify output hosts file: <filename>"
    (modify? #t)
    (hostsfile-out-path filename)]
   #:multi
   [("-l" "--list")
    "list known sources in the hostsfile specified"
    (define list-sources-dec
      (λ (x)
        (list-sources x (current-output-port))))
    (modifiers (add-modifier list-sources-dec (modifiers)))]
   [("-a" "--add") source name
    "add a local or remote source: <source> identified by <name>"
    (modify? #t)
    (define add-source-dec
      (λ (x) (add-source x source name)))
    (modifiers (add-modifier add-source-dec (modifiers)))]
   [("-r" "--remove") source
    "remove a source: <source>"
    (modify? #t)
    (define remove-source-dec
      (λ (x) (remove-source x source)))
    (modifiers (add-modifier remove-source-dec (modifiers)))]
))


(if (initialize?) (initialize) (main))
