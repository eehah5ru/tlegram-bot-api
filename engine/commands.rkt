#lang racket

(require racket/hash)

(require "update.rkt"
         "parameters.rkt")

(provide (all-defined-out))

;;
;; hash symbol (stete machine name) -> list of handlers
;;

(define command-handlers (hasheq))

;;;
;;; filter - procedure that takes cmd-name
;;;
(define (add-command-handler fsm-id a-filter a-handler)
  (set! command-handlers (hash-update command-handlers
                                      fsm-id
                                      (lambda (v)
                                        (cons (cons a-filter a-handler) v))
                                      (list))))

;;;
;;; returns handler or false
;;;
(define (command-handler fsm-id)
  (define (handler-or-false x)
    (match x
      [(cons a-filter a-handler)
       (if (a-filter)
           a-handler
           #f)]
      [_ (error "bad command handler format")]))

  (define handlers (filter procedure?
                           (map handler-or-false
                                (hash-ref command-handlers fsm-id (list)))))

  (if (empty? handlers)
      #f
      (car handlers)))

;;;
;;; command-data helpers
;;;

(define (current-cmd-name)
  (match (update)
    [(command-update cmd-name _ _)
     cmd-name]
    [_
     (error "can't get cmd-name from non-command update")]))

(define (current-cmd-name=? pat)
  (string=? pat (current-cmd-name)))

(define (current-cmd-params)
  (match (update)
    [(command-update _ params _)
     params]
    [_
     (error "can't get params from non-command update")]))
