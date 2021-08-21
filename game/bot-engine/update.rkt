#lang racket
(require nested-hash)

(require (prefix-in msg: net/telebot/message))

(provide (all-defined-out))

(struct message-update (message))

(struct command-update (cmd-name params message))

(struct bkg-job-update (chat-id))

(define (update->message an-update)
  (match an-update
    [(message-update m)
     m]
    [(command-update _ _ m)
     m]
    [(bkg-job-update chat-id)
     ;; minimal message
     (hasheq 'chat (hasheq 'id chat-id))]
    [_
     (error "unsupported update type. cannot be converted to the message")]))

(define (get-chat-id-from-update an-update)
  (match an-update
    [(message-update mess)
     (nested-hash-ref mess 'chat 'id)]
    [(command-update _ _ m)
     (nested-hash-ref m 'chat 'id)]
    [(bkg-job-update chat-id)
     chat-id]
    [_
     (error "unsupported update type to retrieve chat-id")]))
