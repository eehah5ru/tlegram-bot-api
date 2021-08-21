#lang racket

(require racket/pretty
         behavior/fsm
         behavior/reporter
         )

(require "context.rkt"
         "parameters.rkt"
         (prefix-in ef: "effects.rkt")
         )



(provide (all-defined-out))

(define *state-machines* (hasheq))

(define (register-state-machine machine)
  (set! *state-machines*
        (hash-set *state-machines*
                  (state-machine-name machine)
                  machine)))

(define (get-registred-state-machine name)
  (hash-ref *state-machines* name))


(define (make-and-register-state-machine name
                                         states
                                         transitions
                                         [additional-events (list)])
  (define machine (make-state-machine name states transitions additional-events))

  (register-state-machine machine)

  machine)

;;;
;;; execution -> hash
;;;
(define (serialize-machine an-execution)
  (define data (hasheq 'model (symbol->string (state-machine-name (machine-execution-model an-execution)))
                       'condition (symbol->string (machine-execution-condition an-execution))
                       'current-state (symbol->string (state-name (machine-execution-current-state an-execution)))))
  data)

;;;
;;; hash -> execution
;;;
(define (deserialize-machine serialized)
  (define machine (get-registred-state-machine (string->symbol (hash-ref serialized 'model))))
  (define condition (string->symbol (hash-ref serialized 'condition)))
  (define current-state (hash-ref (state-machine-states machine) (string->symbol (hash-ref serialized 'current-state))))

  (private-machine-execution machine condition current-state (make-null-reporter)))

;;; restore state machines from chat state stored in user-context
(define (restore-state-machines!)
  (let* (
         ;; get them from chat-state
         [prev-state-machines (map deserialize-machine
                                   (hash-ref (context-chat-state (user-context)) 'state-machines '()))]
         ;; inject default state machine if there are no any
         [prev-state-machines (if (not (empty? prev-state-machines))
                                  prev-state-machines
                                  (list
                                   (machine-execution-start (make-machine-execution (default-state-machine) (make-port-reporter (current-error-port))))))])
    ;; TODO: deserialize them
    (set-context-state-machines! (user-context) prev-state-machines)
    ))

;;; save state machines to the user-context -> chat-context
(define (save-state-machines!)
  (let* ([current-state-machines (map serialize-machine
                                      (context-state-machines (user-context)))]
         [chat-state (context-chat-state (user-context))]
         [chat-state (hash-set chat-state 'state-machines current-state-machines)])
    ;; TODO serialize them
    (set-context-chat-state! (user-context) chat-state)
    ))

(define (current-state-machine)
  (if (empty? (context-state-machines (user-context)))
      (error "there are no state machines")
      (car (context-state-machines (user-context)))))

(define (set-current-state-machine machine)
  (set-context-state-machines! (user-context)
                               (cons machine (cdr (context-state-machines (user-context))))))

(define (update-current-state-machine updater)
  (let* ([state-machine (current-state-machine)]
         [state-machine (updater state-machine)])
    (set-current-state-machine state-machine)))

;;;
;;; machine should be an execution
;;; if it is a state machine it will be started and execution pued to the state-machines in the context
;;;
(define (push-state-machine machine)
  (let* ([machine (match machine
                   [(? state-machine? _)
                    (machine-execution-start (make-machine-execution machine))]
                   [(? machine-execution? _)
                    machine]
                   [_ (error "unknown machine type. don't know what to push into state-machines")])]
         [machines (context-state-machines (user-context))]
         [new-machines (cons machine machines)])
    (set-context-state-machines! (user-context) new-machines)))

;;;
;;;
;;; POP STATE MACHINE
;;;
;;;
(define (pop-state-machine)
  (set-context-state-machines! (user-context) (cdr (context-state-machines (user-context)))))
