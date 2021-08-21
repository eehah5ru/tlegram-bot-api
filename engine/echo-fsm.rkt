#lang racket
(require          behavior/fsm
                  behavior/reporter

                  (prefix-in effect: "effects.rkt")
                  "update.rkt"
                  "parameters.rkt")

(provide (all-defined-out))

;;;
;;;
;;; very simple replying state machine
;;;
;;;
(define replying-fsm
  (make-state-machine
   'replying-fsm
   (list (make-state 'waiting-for-user-input
                     'start
                     #:execute (lambda (_)
                                 (displayln "waiting")))
         (make-state 'replying-to-the-message
                     'normal
                     #:execute (lambda (_)
                                 (displayln "replying")
                                 (effect:emit-effect 'reply-to-message (string-append (hash-ref (update->message (update)) 'text) " -> ok"))
                                 (effect:emit-effect 'event 'wait-for-user-input)))
         (make-state 'cannot-answer-to-the-command
                     'normal)
         (make-state 'finished
                     'final))
   (list (make-transition 'waiting-for-user-input
                          'replying-to-the-message
                          #:on-event 'message)
         (make-transition 'waiting-for-user-input
                          'cannot-answer-to-the-command
                          #:on-event 'command)
         (make-transition 'replying-to-the-message
                          'waiting-for-user-input
                          #:on-event 'wait-for-user-input)
         (make-transition 'cannot-answer-to-the-command
                          'waiting-for-user-input
                          #:on-event 'wait-for-user-input))))
