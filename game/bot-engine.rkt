#lang racket

(require net/http-client
         json
         data/heap
         (except-in racket/date date->string)
         (only-in srfi/19 string->date date->string lax-date?)
         racket/struct
         racket/pretty
         nested-hash
         racket/hash
         db/mongodb
         db/mongodb/orm/dict

         behavior/fsm
         behavior/reporter

         errortrace
         (prefix-in bot: net/telebot)

         game/bot-engine/effects
         (prefix-in ef: game/bot-engine/effects)
         (prefix-in et: (submod game/bot-engine/effects types))
         game/bot-engine/parameters
         game/bot-engine/echo-fsm
         game/bot-engine/update
         game/bot-engine/current-update
         game/bot-engine/context
         game/bot-engine/chat-state
         (prefix-in sm: game/bot-engine/state-machine)
         net/telebot/message
         (prefix-in cmd: game/bot-engine/commands))


(provide (all-defined-out)
         (all-from-out game/bot-engine/parameters))


;;;
;;;
;;; effects handlers
;;; effects could be
;;; - send message
;;; - push new state machine
;;; - emit event
;;;

(define (process-effects bot)
  (match (context-effects (user-context))
    ;; no more effects to process
    [(list)
     (void)]
    ;; get first and process it
    [(list next-effect rest-effects ...)
     (begin
       (set-context-effects! (user-context) rest-effects)
       (process-effect bot next-effect)
       (process-effects bot)
       (void))]))

;;; FIXME: pass errors to "then" function as well as ok result!
(define (process-effect bot effect)
  (define (handle-bot-is-blocked-by-the-user e)
    (log-warning "send-message: user ~a blocked the bot: ~a" (exn-message e)))

  (match effect
    [(et:send-message then whom message)
     (begin
       (log-info (string-append "sending message..."))
       (with-handlers
         ([bot:exn:telebot:blocked-by-user? handle-bot-is-blocked-by-the-user])

         (let ([sent-message (bot:send-message bot whom message)])
          (when then
            (then sent-message)))))]

    [(et:send-message* then whom message)
     (begin
       (log-info "sending message via send-message*")

       (with-handlers
         ([bot:exn:telebot:blocked-by-user? handle-bot-is-blocked-by-the-user]
          ;; silently eat all errors!
          [exn:fail? (lambda (e)
                       (log-error "send-message*: error sending message: ~a" (exn-message e)))]
          )

         (let ([sent-message (bot:send-message* bot whom message)])
          (log-info "send-message*: message sent")
          (when then
            (then sent-message)))))]

    [(et:send-photo then whom photo)
     (begin
       (log-info "sending photo...")

       (with-handlers
         ([bot:exn:telebot:blocked-by-user? handle-bot-is-blocked-by-the-user])

         (let ([sent-message (bot:send-photo bot whom photo)])
          (when then
            (then sent-message)))))]

    [(et:send-file then whom a-file)
     (begin
       (log-info "sending file...")

       (with-handlers
         ([bot:exn:telebot:blocked-by-user? handle-bot-is-blocked-by-the-user])

         (let ([sent-message (bot:send-file bot whom a-file)])
          (when then
            (then sent-message)))))]

    [(et:forward-message then whom message)
     (begin
       (log-info (string-append "forwarding message..."))

       (with-handlers
         ([bot:exn:telebot:blocked-by-user? handle-bot-is-blocked-by-the-user])

         (let ([sent-message (bot:forward-message bot whom message)])
          (when then
            (then sent-message)))))]

    [(et:reply-to-message then msg)
     (begin
       (log-info (string-append "replying to message: " (message-text msg)))

       (with-handlers
         ([bot:exn:telebot:blocked-by-user? handle-bot-is-blocked-by-the-user])

         (let ([sent-message (bot:reply-to bot (update->message (update)) msg)])
          (when then
            (then sent-message)))))]

    [(et:event then event-id)
     (begin
       (log-info (string-append "handling emitted event: " (symbol->string event-id)))
       (sm:update-current-state-machine (curryr handle-event event-id)))]

    ;;
    ;; PUSH STATE MACHINE
    ;;
    [(et:push-state-machine then a-state-machine)
     (begin
       (log-info (string-append "push state machine:" (symbol->string (state-machine-name a-state-machine))))
       (sm:push-state-machine a-state-machine))]

    ;;
    ;; POP STATE MACHINE
    ;;
    [(et:pop-state-machine then event-id)
     (begin
       (log-info "pop state machine")
       (sm:pop-state-machine)
       (when event-id
         (ef:event event-id)))]

    ;; catch unknown effect
    [(et:effect _)
     (error (string-append "unknown effect type"))]

    [_
     (error "malformed effect format")]))


(define (get-command-handler state-machines)
  (if (empty? state-machines)
      (default-command-handler)
      (let* ([cur-state-machine (car state-machines)]
             [fsm-id (state-machine-name (machine-execution-model cur-state-machine))]
             [handler (cmd:command-handler fsm-id)])
        (if handler
            handler
            (get-command-handler (cdr state-machines))))))
;;;
;;;
;;; initialize bot
;;;
;;;
(define (main)
  (let* ([token (string-trim (file->string (build-path (current-directory) "token")))]
         [state (hasheq)]
         [bot (bot:mk-tg-bot token
                             state)])

    (unless (default-state-machine)
      (error "default state machine is not specified!"))

    ;; ;;
    ;; ;; start handler
    ;; ;;
    ;; (bot:add-command-handler
    ;;  bot
    ;;  ;; filter
    ;;  (lambda (cmd-name)
    ;;    (string=? cmd-name "start"))
    ;;  ;; handler
    ;;  (lambda (bot cmd-name params message)
    ;;    (bot:reply-to bot message "this is start")))

    ;; ;;
    ;; ;; info handler
    ;; ;;
    ;; (bot:add-command-handler
    ;;  bot
    ;;  (lambda (cmd-name)
    ;;    (string=? cmd-name "info"))

    ;;  (lambda (bot cmd-name params message)
    ;;    (cond
    ;;      [(string=? "state" params)
    ;;       ;; (bot:reply-to bot message (jsexpr->string (bot-state bot)))
    ;;       ;; not implemented
    ;;       ]
    ;;      [else
    ;;       ;; (bot:reply-to bot message (jsexpr->string (bot:get-me bot)))
    ;;       ;; (bot:reply-to bot message (jsexpr->string message))
    ;;       (bot:reply-to bot
    ;;                     message
    ;;                     (string-append "id of the chat with me: "
    ;;                                    (number->string (nested-hash-ref message 'chat 'id))))
    ;;       ])))


    ;;
    ;; pass command to the state machine
    ;;
    (bot:add-command-handler
     bot
     ;; filter - pass any
     (lambda (_) #t)

     ;; handler
     (lambda (bot cmd-name params message)
       (parameterize ([update (command-update cmd-name params message)]
                      [user-context (make-context)])
         (restore-chat-state! bot)
         (sm:restore-state-machines!)
         ((get-command-handler (context-state-machines (user-context))))
         (process-effects bot)
         (sm:save-state-machines!)
         (save-chat-state! bot)
         ))
     )

    ;;
    ;; pass message to the state machine
    ;;
    (bot:add-message-handler
     bot
     (lambda (message)
       #t)

     (lambda (message)
       (log-info "handling message")
       ;; make minmal context
       (parameterize ([update (message-update message)]
                      [user-context (make-context)])
         (restore-chat-state! bot)
         (sm:restore-state-machines!)
         (sm:update-current-state-machine (curryr handle-event 'message))
         (process-effects bot)
         (sm:save-state-machines!)
         (save-chat-state! bot))))

    ;;
    ;; set background job handler
    ;;
    (bot:set-background-job-handler
     bot
     (lambda (bkg-job)
       (log-info "handling background-job")
       (parameterize ([update (bkg-job-update (bot:bkg-job-chat-id bkg-job))]
                      [user-context (make-context)])
         (restore-chat-state! bot)
         (sm:restore-state-machines!)
         ;; exec bkg job proc that carry all logic
         ((bot:bkg-job-proc bkg-job))
         (process-effects bot)
         (sm:save-state-machines!)
         (save-chat-state! bot))))

    ;; Start the loop
    (bot:poll bot)))
