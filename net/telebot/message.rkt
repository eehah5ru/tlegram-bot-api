#lang racket

(require nested-hash
         racket/hash
         racket/pretty)

(provide (all-defined-out))

(define (merge-hash v . xs)
  (apply hash-union
         #:combine/key (lambda (_ __ v2) v2)
         (for/list ([h (filter (lambda (v) (not (void? v)))
                               (cons v xs))])
           (for/hasheq ([(k v) h])
             (values k v)))))


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
                        (raise-argument-error 'payload
                                              "cannot convert value to the message"
                                              payload))])])
    (merge-hash message
                (when chat-id
                  (hasheq 'chat_id chat-id))
                (when reply-to-message-id
                  (hasheq 'reply_to_message_id reply-to-message-id))
                (when reply-markup
                  (hasheq 'reply_markup reply-markup))
                )))

;;
;;
;; predicates about message
;;
;;
(define (message-text? msg)
  (hash-has-key? msg 'text))

(define (message-animation? msg)
  (hash-has-key? msg 'animation))

(define (message-audio? msg)
  (hash-has-key? msg 'audio))

(define (message-document? msg)
  (hash-has-key? msg 'document))

(define (message-sticker? msg)
  (hash-has-key? msg 'sticker))

(define (message-photo? msg)
  (hash-has-key? msg 'photo))

(define (message-video? msg)
  (hash-has-key? msg 'video))

(define (message-video-note? msg)
  (hash-has-key? msg 'video_note))

(define (message-voice? msg)
  (hash-has-key? msg 'voice))

(define (message-file? msg)
  (or (message-animation? msg)
      (message-audio? msg)
      (message-document? msg)
      (message-photo? msg)
      (message-video? msg)
      (message-video-note? msg)
      (message-voice? msg)))

(define (message-file-field-name msg)
  (cond
    [(not (message-file? msg)) #f]
    [(message-animation? msg) 'animation]
    [(message-audio? msg) 'audio]
    [(message-document? msg) 'document]
    [(message-photo? msg) 'photo]
    [(message-video? msg) 'video]
    [(message-video-note? msg) 'video_note]
    [(message-voice? msg) 'voice]
    [else (raise-argument-error 'msg "should be a file based message type. but gt unknown one." msg)]))

(define (message-send-method-name msg)
  (cond
    [(message-text? msg) "sendMessage"]
    [(message-animation? msg) "sendAnimation"]
    [(message-audio? msg) "sendAudio"]
    [(message-document? msg) "sendDocument"]
    [(message-photo? msg) "sendPhoto"]
    [(message-video? msg) "sendVideo"]
    [(message-video-note? msg) "sendVideoNote"]
    [(message-voice? msg) "sendVoice"]
    [else (raise-argument-error 'msg "should be supported type" msg)]))

;;
;; flatten message to payload suitable to send
;;
(define (message->payload msg)
  (define (flatten-file-fields msg field-name)
    (hash-set msg field-name (nested-hash-ref msg
                                              field-name 'file_id
                                              #:default (hash-ref msg field-name))))

  ;; TODO: process different photo sizes
  (define (flatten-photo-field msg)
    (hash-set msg
              'photo
              (hash-ref (vector-ref (hash-ref msg
                                              'photo)
                                    0)
                        'file_id
                        (hash-ref msg 'photo))))
  (cond
    [(message-text? msg) msg]
    [(message-photo? msg) (flatten-photo-field msg)]
    [(message-file? msg) (flatten-file-fields msg (message-file-field-name msg))]
    [else (raise-argument-error 'msg "should be suppored type. By now its is either text or file based payload types" msg)]))


(define (message-text msg)
  (hash-ref (make-message msg) 'text))
