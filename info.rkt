#lang info
(define collection 'multi)
(define deps '("base"
               "nested-hash"
               "errortrace"
               "data-lib"
               "http-easy"
               "https://github.com/eehah5ru/behavior.git"
               "mongodb"
               "collections"
               ))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/telegram-bot-api.scrbl" ())))
(define pkg-desc "tg bot api with engine for narratives")
(define version "0.1")
(define pkg-authors '(eehah5ru eeefff))
