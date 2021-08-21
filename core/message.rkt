#lang racket

(require nested-hash
         racket/hash
         racket/pretty)


(provide (all-defined-out))

(define (merge-hash v . xs)
  (apply hash-union
         #:combine/key (lambda (_ __ v2) v2)
         (filter (lambda (v) (not (void? v)))
                 (cons v xs))))


;;;
;;;
;;; keyboard button
;;;
;;;
(define (make-keyboard-button text)
  (hasheq 'text (match text
                  [(? string? _) text]
                  [(? procedure? _) (text)]
                  [_ (error "unknown type of the button content")])))


;;;
;;;
;;; reply keyboard markup
;;;
;;;
(define (make-reply-keyboard-markup keyboard
                                    #:resize-keyboard [resize-keyboard #f]
                                    #:one-time-keyboard [one-time-keyboard #f]
                                    #:input-field-placeholder [input-field-placeholder #f]
                                    #:selective [selective #f])
  (let ([reply-markup (hasheq 'keyboard keyboard)])
    (merge-hash reply-markup
                (hasheq 'resize_keyboard resize-keyboard
                        'one_time_keyboard one-time-keyboard
                        'selective selective)
                (match input-field-placeholder
                  [(? false? _) (void)]
                  [(? procedure? _) (hasheq 'input_field_placeholder (input-field-placeholder))]
                  [_ (hasheq 'input_field_placeholder input-field-placeholder)]))))

;;;
;;;
;;; message
;;;
;;;
;; make-message -- payload: text or jsexpr --> jsexpr
(define (make-message payload
                      #:chat-id [chat-id #f]
                      #:reply-to-message-id [reply-to-message-id #f]
                      #:reply-markup [reply-markup #f])
  (let ([message (match payload
                   [(? string? text) (hasheq 'text text)]
                   [(? hash? payload) payload]
                   ;; i18n string
                   [(? procedure? payload) (hasheq 'text (payload))]
                   [_ (begin
                        (pretty-print payload)
                        (error "cannot convert value to the message"))])])
    (merge-hash message
                (when chat-id
                  (hasheq 'chat_id chat-id))
                (when reply-to-message-id
                  (hasheq 'reply_to_message_id reply-to-message-id))
                (when reply-markup
                  (hasheq 'reply_markup reply-markup))
                )))

(define (message-text msg)
  (hash-ref (make-message msg) 'text))
