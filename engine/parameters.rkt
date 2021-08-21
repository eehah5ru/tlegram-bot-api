#lang racket

(provide (all-defined-out))
;;
;;
;; current update
;;
;;
(define update (make-parameter #f))

;;
;;
;; users's context
;;
;;
(define user-context (make-parameter #f))

;;; TODO: replace fsm to more pingpongish machine
;;; actually this is starting point for bot logic
(define default-state-machine (make-parameter #f))

;;
;; default command handler
;;
(define default-command-handler (make-parameter #f))
