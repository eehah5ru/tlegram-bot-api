#lang racket

(require racket/pretty
         racket/hash
         db/mongodb
         db/mongodb/orm/dict
         (prefix-in coll: data/collection))

(provide (all-defined-out)
         (except-out mongo-chat-state-query))


(define (mongo-chat-state-query chat-id)
  (define query (hasheq 'chat-id chat-id))

  (define chat-states (sequence->list (mongo-dict-query "chatStates" query)))

  (match chat-states
    [(list) (error 'mongo-chat-state-query "no mongo dics for chat-id ~a" chat-id)]
    [(list v) v]
    [_ (error 'mongo-chat-state-query "many mongo dicts for unique chat-id ~a" chat-id)]))



(define (mongo-chat-state-exist? chat-id)
  (define query (hasheq 'chat-id chat-id))

  (define chat-states (sequence->list (mongo-dict-query "chatStates" query)))

  (match chat-states
    [(list) #f]
    [(list _) #t]
    [_ (error 'mongo-chat-state-exist? "many mongo dicts for unique chat-id ~a" chat-id)]))

;; save chat state
(define (mongo-save-chat-state chat-id chat-state)
  ;; mongo dict
  (define saved (if (mongo-chat-state-exist? chat-id)
                    (mongo-chat-state-query chat-id)
                    (create-mongo-dict "chatStates")))

  ;; keys in old state
  (define old-state-keys (list->set (hash-keys (unsequence-mongo-dict saved))))
  ;; keys of new state
  (define new-state-keys (list->set (hash-keys chat-state)))

  (define orphan-keys (set-subtract old-state-keys new-state-keys))

  (for ([key orphan-keys])
    (mongo-dict-remove! saved key))

  (for ([(name value) chat-state])
    (mongo-dict-set! saved name value)))

;;; unbox mongo dict from sequences
(define (unsequence-mongo-dict dict)
  (define (unsequence x)
    (match x
      [(? coll:collection? _)
       (coll:sequence->list (coll:map unsequence
                                      x))]
      [(? vector? _)
       (map unsequence
            (vector->list x))]
      [_ x]))

  (for/hasheq ([(k v) dict])
    (values k (unsequence v))))

;;; read chat state from mongo
(define (mongo-query-chat-state chat-id)
  (define query (hasheq 'chat-id chat-id))

  (define chat-states (sequence->list (mongo-dict-query "chatStates" query)))

  (define chat-state (match chat-states
                       [(list) (hasheq 'chat-id chat-id)]
                       [(list v) v]
                       [_ (error "many states for one chat-id")]))

  (define result (unsequence-mongo-dict chat-state))

  result)
