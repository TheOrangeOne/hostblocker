#lang racket


(require "hostsfile.rkt")

(provide hostsfile-path)

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


(define (decorate hf fns)
  (cond [(empty? fns) hf]
        [else
         (decorate ((first fns) hf) (rest fns))]))


(define (main)
  (define in (open-input-file (hostsfile-path)))
  (define myhostsfile (hostsfile-parse in))
  (close-input-port in)

  (define dec-hf (decorate myhostsfile (reverse (modifiers))))

  (cond
    [(modify?)
     (define hostsfile-out
       (open-output-file (hostsfile-out-path) #:exists 'replace))
     (hostsfile-write dec-hf hostsfile-out)
     (close-output-port hostsfile-out)]
    [(empty? (vector->list (current-command-line-arguments)))
     (hostsfile-write myhostsfile (current-output-port))]
    [else
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


(define (add-modifier mod mods)
  (cons mod mods))

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
   #:multi
   [("-l" "--list")
    "list known sources in the hostfile specified"
    (define list-sources-dec
      (λ (x)
        (hostsfile-list-sources
         x (current-output-port) (hostsfile-path))))
    (modifiers (add-modifier list-sources-dec (modifiers)))]
   [("-t" "--tags")
    "list all the tags in the hostfile specified"
    (define list-tags-dec
      (λ (x)
        (hostsfile-list-tags x (current-output-port) (hostsfile-path))))
    (modifiers (add-modifier list-tags-dec (modifiers)))]
   [("-a" "--add-source") source
    "add a local or remote source: <source>"
    (modify? #t)
    (define add-source-dec
      (λ (x) (hostsfile-add-new x source)))
    (modifiers (add-modifier add-source-dec (modifiers)))]

   [("-r" "--remove-source") source
    "remove a local or remote source: <source>"
    (modify? #t)
    (define remove-source-dec
      (λ (x) (hostsfile-remove-source x source (hostsfile-path))))
    (modifiers (add-modifier remove-source-dec (modifiers)))]
))


(main)
