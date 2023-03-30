#lang errortrace racket

(require
         json
         data/heap
         (except-in racket/date date->string)
         (only-in srfi/19 string->date date->string lax-date?)
         nested-hash
         racket/struct
         net/http-easy

         net/telebot/message
         net/telebot/mongo-persistance)

(provide (all-defined-out))

(define *api-domain* "api.telegram.org")

;;; enable / disable mongo persistance for chat/user states between updates
(define mongo-persistance-enabled (make-parameter #f))

;;;
;;;
;;; exceptions
;;;
;;;

;;;
;;; general exception for all telebot's errors
;;;
(struct exn:telebot exn:fail ())

(struct exn:telebot:blocked-by-user exn:telebot ())


(define (raise-telebot-error msg
                             [parent-error #f]
                             #:mk-error [mk-error exn:telebot])
  (define err-message (string-append (format "telebot error: ~a" msg)
                         (if parent-error
                             (format " / parent error: ~a" (exn-message parent-error))
                             "")))
  (raise (mk-error err-message (current-continuation-marks))))


(define (raise-telebot-blocked-by-user msg
                                       [parent-error #f])
  (raise-telebot-error msg parent-error #:mk-error exn:telebot:blocked-by-user))

;;;
;;;
;;; STATE
;;; state is simple hash
;;;
(define (make-state)
  (hasheq 'chats (hasheq)
          'users (hasheq)))

(define (chat-state bot chat-id)
  (if (mongo-persistance-enabled)
      ;; return from mongo
      (mongo-query-chat-state chat-id)

      ;; use internal tg-bot state
      (nested-hash-ref (tg-bot-state bot) 'chats chat-id
                   #:default (hasheq 'chat-id chat-id))))

;;; updates whole bot structure
(define (set-chat-state bot chat-id state)
  (if (mongo-persistance-enabled)
      ;; save to mongo
      (mongo-save-chat-state chat-id state)

      ;; save to tg-bot structure
      (let ([new-state (nested-hash-set (tg-bot-state bot) 'chats chat-id state
                                     #:hash hasheq)])
     (set-tg-bot-state! bot new-state))))

;;;
;;; functionally update chat state
;;;
(define (update-chat-state bot chat-id updater)
  (define c-state (chat-state bot chat-id))
  (define new-c-state (updater c-state))

  (set-chat-state bot chat-id new-c-state))

;;;
;;;
;;; TG BOT
;;;
;;;
(struct tg-bot (token
                [offset #:mutable]
                [queue #:mutable]
                [command-handlers #:mutable]
                [message-handlers #:mutable]
                [bkg-job-handler #:mutable]
                [state #:mutable]))


;; Creates and returns a `tg-bot' initialized with `token' and `admin'.
(define (mk-tg-bot token [state (make-state)])
  (tg-bot token 0
          ;; queue should be a min-heap of (scheduled_time . thunk)
          (make-heap (lambda (a b) (<= (car a) (car b))))
          ;; list of command handlers
          (list)
          ;; list of message handlers
          (list)
          ;; there is no bkg-job-handler by default
          #f
          ;; state should be another hash - this time for random variables
          state))

;; heap-pop-min! -- heap: heap --> heap element
;; Utility function to remove the minimum from the given `heap' **and**
;; return its value.
(define (heap-pop-min! heap)
  (begin0
   (heap-min heap)
   (heap-remove-min! heap)))

;; heap-peek -- heap: heap --> boolean
;; Returns the topmost element of `heap', or #f if `heap' is empty.
(define (heap-peek heap)
  (if (> (heap-count heap) 0)
      (heap-min heap)
      #f))

(define error-value
  (case-lambda
   [(what) `(error ,what)]
   [(what more)
    `(error ,what ,(cond [(list? more) (format "~a" more)]
                         [(exn? more)  (format "(~a)" (exn-message more))]
                         [else (format "(~a)" more)]))]))

;; with-timeout -- timeout: number, thunk: thunk
;; Shamelessly stolen from <https://github.com/racket/racket/blob/master/racket/collects/version/check.rkt#L22>
;; Takes a number of seconds and a `thunk', and executes the `thunk' with
;; the given `timeout', returning the result of its execution.
(define (with-timeout timeout thunk)
  (define result #f)
  (define r
    (sync/timeout
     timeout
     (thread (λ ()
               (set! result
                     (with-handlers
                      ([void (λ (e)
                               (error-value "internal error" e))])
                      (thunk)))))))
  (if r result (error-value "timeout")))

;; first-rest -- string --> string, string
;; Splits a string in first and rest, where first is all the characters up
;; to the first whitespace character.
(define (first-rest string)
  (let ([match (regexp-match #px"(.+?) +(.+)" string)])
    (when match
      (values (second match) (third match)))))

;; dateify -- when: date or lax-date --> date
;; If passed a lax-date, turns it into a date.
(define (dateify when)
  (if (lax-date? when)
      (let* ([now (seconds->date (current-seconds))]
             [unpacked (struct->list when)]
             [hours (fourth unpacked)]
             [minutes (third unpacked)])
        (struct-copy date now
                     [hour hours]
                     [minute minutes]
                     [second 0]))
      when))

;; try-string->timestamp -- string: string, datefmt: string --> integer
(define (try-string->timestamp string datefmt)
  (with-handlers ([exn:fail? (lambda (ex) #f)])
    (date->seconds (dateify (string->date string datefmt)))))

;; try-string->number -- string: any --> integer
;; Tries to parse string as a number, or returns 0.
(define (try-string->number string)
  (with-handlers ([exn:fail? (lambda (ex) 0)])
    (or (string->number string) 0)))

;; timedelta-->timestamp -- string: string --> integer
;; Returns the timestamp from now to the given time offset, specified in
;; the [DDd][HHh][MMm] format.  An empty offset will simply result in now
;; being returned.
(define (timedelta->timestamp string)
  (with-handlers ([exn:fail? (lambda (ex) #f)])
    (let* ([match (regexp-match #px"^(?:(\\d+)d)?(?:(\\d+)h)?(?:(\\d+)m)?$" string)]
           [days (try-string->number (second match))]
           [hours (try-string->number (third match))]
           [minutes (try-string->number (fourth match))])
      (+ (current-seconds)
         (* days 86400)
         (* hours 3600)
         (* minutes 60)))))

;; parse-time-string -- when: string
;; Tries to parse a time string with various strategies
(define (parse-time-string when)
  (cond
    [(try-string->timestamp when "~Y-~m-~d")]
    [(try-string->timestamp when "~H:~M")]
    [(timedelta->timestamp when)]
    [else #f]))

;; api-call -- bot: tg-bot, endpoint: string, payload: optional jsexpr --> jsexpr
;; Makes an API call to the Telegram servers with the given `payload'.
(define (api-call bot
                  endpoint
                  (payload '())
                  (a-file #f)
                  #:file-field-name [file-field-name "photo"])

  (with-handlers ([exn:fail:http-easy:timeout?
                   (lambda (e)
                     (raise-telebot-error (format "~a: timeout error" endpoint) e))]
                  [exn:fail:http-easy?
                   (lambda (e)
                     (raise-telebot-error "http-level error" e))])
    (begin
      (define resp
        (post
         (string-append "https://api.telegram.org" "/bot" (tg-bot-token bot) "/" endpoint)
         ;; #:headers (hasheq 'Content-Type "application/json")
         #:data (if a-file
                    (multipart-payload
                     (field-part "chat_id" (number->string (hash-ref payload 'chat_id "")))
                     (field-part "form" (jsexpr->string payload))
                     (file-part file-field-name a-file))
                    (json-payload payload))
         ))
      (match resp
        ;; ok
        [(response #:status-code 200)
         (let [(r (response-json resp))]
           r)]

        ;; blocked
        [(response #:status-code 403)
         (begin
           (log-warning "api call returns 403 code: ~a" (bytes->string/utf-8 (response-body resp)))
           (raise-telebot-blocked-by-user
            (format "api call returns 403 code: ~a"
                    (bytes->string/utf-8 (response-body resp)))))
         ]

        ;; other errors
        [_ (raise-telebot-error (string-append "error-in-api-call: " (bytes->string/utf-8 (response-body resp))))]))))


;;;
;;; parse api-call result
;;;
(define (parse-api-call-result result)
  (match result
    [(hash-table ('ok #t))
     (hash-ref result 'result)]
    [_ (raise-telebot-error (string-append "result from tg api is not ok: "
                                           (pretty-format result)))]))

;; (define (api-call-multipart bot payload file_payload)
;;   )

;; get-me -- bot: tg-bot --> jsexpr
(define (get-me bot)
  (api-call bot "getMe"))

;; raw-get-updates -- bot: tg-bot --> jsexpr
;; Returns an hasheq containing up to 100 of the last updates from Telegram.
;; N.B.: in #hasheq(), '("a" "b") or (list "a" "b") != jsexpr, just use ("a" "b")
(define (raw-get-updates bot)
  (api-call bot "getUpdates"
            (hasheq 'offset (tg-bot-offset bot)
                    'limit 100
                    'timeout 10
                    'allowed_updates '("message"))))

;; get-updates -- bot: tg-bot --> list
(define (get-updates bot)
  (let ([updates (raw-get-updates bot)])
    (if (and (hash? updates) (hash-has-key? updates 'result))
        (hash-ref updates 'result)
        (begin
          (sleep 10)
          '()))))

;; clean-updates -- bot: tg-bot --> void (jsexpr)
;; "Flushes" out the update queue for `bot' - avoids things like the bot
;; getting in a shutdown loop.
(define (clean-updates bot)
  (api-call bot "getUpdates"
            (hasheq 'offset (tg-bot-offset bot)
                    'limit 1
                    'timeout 0
                    'allowed_updates '("message"))))


;; send-message -- bot: tg-bot, who: number, payload: string or jsexpr
;; If `payload' is a string, builds an hasheq from it.  It then updates it
;; with `who' (the ID of the chat to send the message to), and does an API
;; call to send the result.
(define (send-message bot who payload)
  (let* ([payload (make-message payload #:chat-id who)])
    (parse-api-call-result (api-call bot
                                     "sendMessage" payload))))

;;;
;;; generic verision of send message
;;; can send messsages of different kind
;;; text - 'text
;;; file based types - according to
(define (send-message* bot who payload)
  (let* ([payload (message->payload payload)]
         [payload (make-message payload #:chat-id who)]
         [file-field-name (if (message-file? payload)
                              (message-file-field-name payload)
                              #f)]
         [send-method (message-send-method-name payload)]
         [a-file (cond
                   [(not (message-file? payload))
                    #f]
                   [(string? (hash-ref payload file-field-name))
                    #f]
                   [(input-port? (hash-ref payload file-field-name))
                    (hash-ref payload file-field-name)]
                   [else
                    (raise-argument-error file-field-name "should be a string? or an input-port?" payload)])])

    (parse-api-call-result (api-call bot
                                     send-method
                                     payload
                                     a-file
                                     #:file-field-name file-field-name))

    ))

;; reply-to -- bot: tg-bot, message: jsexpr, payload: any
;; Dispatches `send-message' to send `payload' in reply to the user who
;; sent the message `message'.
(define (reply-to bot message payload)
  (let* ([who (hash-ref (hash-ref message 'chat) 'id)]
         [msg_id (hash-ref message 'message_id)]
         [payload (make-message payload
                                #:chat-id who
                                #:reply-to-message-id msg_id)])
    (send-message bot who payload)))

;;;
;;; forward message
;;;
(define (forward-message bot who message)
  (let* ([msg_id (hash-ref message 'message_id)]
         [from_chat_id (hash-ref (hash-ref message 'chat) 'id)]
         [payload (hasheq 'message_id msg_id
                          'chat_id who
                          'from_chat_id from_chat_id)])
    (parse-api-call-result (api-call bot "forwardMessage" payload))))

;;;
;;; send photo
;;;
(define (send-photo bot who photo)
  (let* ([payload (hasheq 'chat_id who)]
         [payload (if (string? photo)
                      (hash-set payload 'photo photo)
                      payload)]
         [photo (cond
                  [(string? photo)
                   #f]
                  [(input-port? photo)
                   photo]
                  [else
                   (raise-argument-error 'photo "should be a string? or an input-port?" photo)])])
    (parse-api-call-result (api-call bot "sendPhoto" payload photo))))

;;;
;;; send file
;;;
(define (send-file bot whom a-file)
  (let* ([payload (hasheq 'chat_id whom)]
         [payload (if (string? a-file)
                      (hash-set payload 'document a-file)
                      payload)]
         [a-file (cond
                   [(string? a-file)
                    #f]
                   [(input-port? a-file)
                    a-file]
                   [else
                    (raise-argument-error 'a-file "should be a string? or an input-port?" a-file)])])
    (parse-api-call-result (api-call bot "sendDocument" payload a-file #:file-field-name "document"))))

;;;
;;;
;;;
(define (add-command-handler bot filter function)
  (define handler (lambda (bot cmd-name cmd-params message)
                    (when (filter cmd-name)
                      (function bot cmd-name cmd-params message))))

  (set-tg-bot-command-handlers! bot (cons handler (tg-bot-command-handlers bot))))

(define (add-message-handler bot filter function)
  (define handler (lambda (message)
                    (when (filter message)
                      (function message))))
  (set-tg-bot-message-handlers! bot (cons handler (tg-bot-message-handlers bot))))

(define (set-background-job-handler bot proc)
  (set-tg-bot-bkg-job-handler! bot proc))

;;; TODO: unused?
(define (default-command bot params message)
  (reply-to bot message "Command not understood."))


;; is-text? -- message: jsexpr --> boolean
(define (is-text? message)
  (hash-has-key? message 'text))

(define (is-photo? message)
  (hash-has-key? message 'photo))

(define (is-command? message)
  (and (is-text? message) (regexp-match #px"^/(\\w+)( +(.+))?$" (hash-ref message 'text))))


;;;
;;; handle incoming messages
;;;
(define (handle-message bot message)
  (if (is-command? message)
    (let* ([text (hash-ref message 'text)]
           [match (regexp-match #px"^/(\\w+)( +(.+))?$" text)])
      (if match
          (begin
            ;; The message contains a command - pull out the relevant matches
            (let* ([command (second match)]
                   [params (or (fourth match) "")])
              (for ([handler (tg-bot-command-handlers bot)])
                (with-handlers ([exn:fail? (lambda (e)
                                           ;; (print-error-trace (current-error-port)
                                           ;;                    e)
                                           (println (string-append "error handling command '" command "':" (exn-message e))))])
                  ;; call all matched command handlers
                  (handler bot command params message)))
              ))
          (displayln (string-append "unsupported command syntax: " text))))
    ;; usual messages
    (begin
      (for ([handler (tg-bot-message-handlers bot)])
        (with-handlers ([exn:fail? (lambda (e)
                                     ;; (print-error-trace (current-error-port)
                                     ;;                    e)
                                     (println (string-append "error handling message:" (exn-message e))))])
          (handler message))))))

;;;
;;;
;;; WORKERS
;;;
;;;

(define current-queue-processor-thread (make-parameter #f))

(define (send-to-queue-processor msg [queue-processor-thread #f])
  (thread-send (if queue-processor-thread
                   queue-processor-thread
                   (current-queue-processor-thread))
               msg))

;;;
;;;
;;; worker for gettting updates from telegram
;;;
;;;
(define (get-tg-updates-worker bot queue-processor-thread)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (log-error (format "updates worker error: ~a" (exn-message e)))
                     (send-to-queue-processor (list 'error e) queue-processor-thread))])
    (let loop ()
      (displayln "getting updates from TG..")

      (for ([update (get-updates bot)])
        (log-info "got-update")
        (set-tg-bot-offset! bot (+ (hash-ref update 'update_id) 1))
        (thread-send queue-processor-thread (list 'tg-update update)))

      (loop))))

;;;
;;;
;;; queue processor worker
;;;
;;;
(define (queue-processor-worker bot)
  (let loop ()
    (match (thread-receive)
      ;; exit if done
      ['done (void)]

      ;; exit if error
      [(list 'error e)
       (begin
         (log-error (format "queue processor - got error: ~a" (exn-message e)))
         (raise e))]

      ;; got tg update
      [(list 'tg-update update)
       (begin
         (handle-message bot (hash-ref update 'message))
         (loop))]

      ;; got background job
      [(list 'bkg-job bkg-job)
       (handle-background-job bot bkg-job)
       (loop)]

      ;; unknown queue event
      [_ (begin
           (define msg "unknown message in telebot queue processor")
           (log-error msg)
           (raise-telebot-error msg))]
      )))

;;;
;;;
;;; background workers
;;;
;;;

(struct bkg-job (chat-id proc))

(define background-workers '())

(define (add-background-worker worker-proc)
  (set! background-workers (cons worker-proc background-workers)))


(define (send-background-job chat-id func)
  (thread-send (current-queue-processor-thread) (list 'bkg-job (bkg-job chat-id func))))

(define (handle-background-job bot bkg-job)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-error (string-append "error while handling background job: "
                                                         (exn-message e))))])
    (if (tg-bot-bkg-job-handler bot)
        (begin
          ((tg-bot-bkg-job-handler bot) bkg-job))
        (raise-telebot-error "there is no background job handler registred in bot. fail to handle job."))))


;;;
;;; main loop
;;; single thread processing event's queue
;;;
(define (poll bot)
  (define queue-processor-thread (thread (lambda ()
                                           (queue-processor-worker bot))))
  (define tg-updates-thread (thread (lambda ()
                                      (get-tg-updates-worker bot queue-processor-thread))))

  (parameterize ([current-queue-processor-thread queue-processor-thread])
    (define bkg-worker-threads (map (lambda (worker-proc)
                                      (thread (lambda ()
                                                (worker-proc))))
                                    background-workers))

    (with-handlers ([exn:break?
                     (lambda (e)
                       (log-info "BREAK")
                       (thread-send queue-processor-thread 'done)
                       (thread-wait queue-processor-thread)
                       (for ([bkg-worker-thread bkg-worker-threads])
                         (kill-thread bkg-worker-thread)
                         (thread-wait bkg-worker-thread))
                       (kill-thread tg-updates-thread))])

      (thread-wait queue-processor-thread)
      (for ([bkg-worker-thread bkg-worker-threads])
        (kill-thread bkg-worker-thread)
        (thread-wait bkg-worker-thread))
      (kill-thread tg-updates-thread)
      )))
