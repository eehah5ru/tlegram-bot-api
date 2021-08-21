#lang racket

(require "context.rkt"
         "parameters.rkt"
         "../telebot/message.rkt")

(provide (all-defined-out))

(module types racket
  (provide (all-defined-out))
  (struct effect (then))

  (struct send-message effect (whom message))

  (struct send-photo effect (whom photo))

  (struct send-file effect (whom a-file))

  (struct forward-message effect (whom message))

  (struct reply-to-message effect (message))

  (struct event effect (event-id))

  (struct push-state-machine effect (state-machine))

  (struct pop-state-machine effect (event-id)))

(require (prefix-in types: (submod "." types)))

;;; default event name fired after popping state machine
(define *default-pop-state-machine-event* 'current-state-machine)

(define (effect-id? v)
  (match v
    ['send-message #t]
    ['send-photo #t]
    ['send-file #t]
    ['forward-message #t]
    ['reply-to-message #t]
    ['event #t]
    ['push-state-machine #t]
    ['pop-state-machine #t]
    [_ #f]))

(define/contract (emit-effect an-effect)
  (-> types:effect? void?)
  (let ([effects (context-effects (user-context))])
    (set-context-effects! (user-context) (append effects (list an-effect)))))

(define (send-message whom
                      message
                      #:then [then #f]
                      #:reply-markup [reply-markup #f])
  (emit-effect (types:send-message then
                                   whom
                                   (make-message message
                                                 #:reply-markup reply-markup))))

(define (send-photo whom
                    photo
                    #:then [then #f])
  (emit-effect (types:send-photo then
                                 whom
                                 photo)))

(define (send-file whom
                   a-file
                   #:then [then #f])
  (emit-effect (types:send-file then
                                whom
                                a-file
                                )))

(define (forward-message whom
                         message
                         #:then [then #f])
  (emit-effect (types:forward-message then
                                      whom
                                      (make-message message))))

(define (reply-to-message text
                          #:reply-markup [reply-markup #f]
                          #:then [then #f])
  (emit-effect (types:reply-to-message then
                                       (make-message text
                                                     #:reply-markup reply-markup))))

(define (event event-id #:then [then #f])
  (emit-effect (types:event then
                            event-id)))

(define (push-state-machine a-state-machine #:then [then #f])
  (emit-effect (types:push-state-machine then
                                         a-state-machine)))

(define (pop-state-machine #:then [then #f] #:event [event-id *default-pop-state-machine-event*])
  (emit-effect (types:pop-state-machine then event-id)))
