#lang racket

;;
;;
;; utils to manipulate with current update from tg
;;
;;

(require nested-hash)

(require "parameters.rkt")
(require "update.rkt")

(provide (all-defined-out))

;;;
;;; user's update helpers
;;;

(define (current-update->message)
  (update->message (update)))

(define (current-update->text?)
  (hash-has-key? (current-update->message) 'text))

(define (current-update->text)
  (hash-ref (current-update->message) 'text ""))

(define (current-update->chat-id)
  (nested-hash-ref (current-update->message) 'chat 'id))

(define (match-current-update-text? pattern)
  (if (current-update->text?)
      (regexp-match? pattern (current-update->text))
      #f))
