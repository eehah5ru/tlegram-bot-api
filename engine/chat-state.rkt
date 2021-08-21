#lang racket



(require "parameters.rkt"
         "context.rkt"
         "update.rkt"
         (prefix-in bot: "../telebot/telebot.rkt"))

(provide (all-defined-out))

;;; restore chat state from bot instance
(define (restore-chat-state! bot)
  (let* ([chat-id (get-chat-id-from-update (update))]
         [chat-state (bot:chat-state bot chat-id)])
    (set-context-chat-state! (user-context) chat-state)))

;;; save chat state to the bot instance
(define (save-chat-state! bot)
  (let ([chat-id (hash-ref (context-chat-state (user-context)) 'chat-id)])
    (bot:set-chat-state bot chat-id (context-chat-state (user-context)))))

;;;
;;; user-side state updater
;;;
(define (update-state updater)
  (let* ([chat-state (context-chat-state (user-context))]
        [new-chat-state (updater chat-state)])
    (set-context-chat-state! (user-context) new-chat-state)))
