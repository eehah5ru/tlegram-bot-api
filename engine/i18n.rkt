#lang racket


(require racket/hash
         "context.rkt"
         "parameters.rkt"
         "chat-state.rkt")

(provide (all-defined-out))

;;;
;;; lang labels
;;;
(define LANG-RU "ru")

(define LANG-EN "en")

(define (lang-id? lang)
  (or (string=? LANG-RU lang)
      (string=? LANG-EN lang)))

;;;
;;; return procedure!
;;;
(define (multilang . xs)
  (let ([lang-strings (match xs
                        [(list (? string? s)) (hasheq (default-language) s)]
                        [(list (? hash? s)) s]
                        [(list (? hash? s1) (? hash? s2)) (hash-union s1 s2)]
                        [_ (error "malformed format for multilang string")])])
    (lambda ()
      (let ([lang (current-language)]
            [fallback-lang (default-language)])
        (match lang-strings
          [(hash-table ((== lang) str) _ ...) str]
          [(hash-table ((== fallback-lang) str) _ ...) str]
          [_ (error "cannot get localised string from multilang structure")])))))

(define (ru str)
  (hasheq LANG-RU str))

(define (en str)
  (hasheq LANG-EN str))


;;;
;;; fallback lang
;;;
(define default-language (make-parameter LANG-RU))

;;
;; get current language for the session
;;
(define (current-language)
  (let ([state (context-chat-state (user-context))])
    (hash-ref state 'lang (default-language))))

;;
;; set lang in current user's context
;;


(define/contract (set-language lang)
  (-> lang-id? void?)
  (update-state (lambda (state)
                  (hash-set state 'lang lang))))


;; (let ([a 1])
;;   (match (hasheq 1 2
;;                  3 4)
;;     [(hash-table ((== a) x) _ ...) x]
;;     [_ (error "blah")]))
