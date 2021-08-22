#lang racket

(require nested-hash
         (only-in predicates true?))

(require game/bot-engine/parameters)
(require game/bot-engine/current-update)

(provide (all-defined-out))

(define *owners-path* (build-path (current-directory) "owners"))
(define *admins-path* (build-path (current-directory) "admins"))

(define (owners)
  (file->list *owners-path*))

(define (admins #:include-owners? [include-owners? #f])
  (define only-admins (file->list *admins-path*))
  (if include-owners?
      (append (owners) only-admins)
      only-admins))

(define (add-admin admin-id)
  (with-output-to-file *admins-path*
    #:mode 'text
    #:exists 'replace
    (lambda ()
      (for ([admin (append (admins) (list admin-id))])
        (printf "~a\n" admin)))))

(define (remove-admin admin-id)
  (define updated-admins (remove admin-id (admins)))

  (with-output-to-file *admins-path*
    #:mode 'text
    #:exists 'replace
    (lambda ()
      (for ([admin updated-admins])
        (printf "~a\n" admin)))))

(define (is-owner? who)
  (true? (member who (owners))))

(define (is-admin? who #:include-owners? [include-owners? #f])
  (or (is-owner? who)
      (true? (member who (admins #:include-owners? include-owners?)))))

;;;
;;; check if current update message is from one of admins?
;;;
(define (by-admin? #:include-owners? [include-owners? #f])
  (let ([from (nested-hash-ref (current-update->message) 'chat 'id)])
    (is-admin? from
               #:include-owners? include-owners?)))

(define (by-owner?)
  (let ([from (nested-hash-ref (current-update->message) 'chat 'id)])
    (is-owner? from)))

;;;
;;; check newcessary files exist on module loading phase
;;;
(begin
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (displayln (string-append "error opening admins or owners file: "
                                               (exn-message e))))])
    (owners)
    (admins)))
