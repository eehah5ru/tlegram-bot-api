(module context racket
  (provide (all-defined-out))

  (struct context ([state-machines #:mutable]
                   [effects #:mutable]
                   [chat-state #:mutable])
    #:transparent)

  (define (make-context [state-machines '()] [effects '()] [chat-state (hasheq)])
    (context state-machines effects chat-state))
  )
