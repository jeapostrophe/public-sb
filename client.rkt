#lang racket/base
(require racket/cmdline
         racket/tcp
         racket/match
         racket/system)

(define (*system args)
  (printf "~a\n" args)
  (system args))

(define (client-connect server port dir)
  (define-values (from to) (tcp-connect server port))
  (define sounds 
    (sort (map path->string (directory-list dir))
          string-ci<=?))

  (write sounds to)
  (flush-output to)

  (let loop ()
    (match (read from)
      [(? eof-object?)
       (error 'client "Server disconnected")]
      [(? string? some-sound)
       (when (member some-sound sounds)
         (*system (format "mplayer '~a'" (build-path dir some-sound))))
       (loop)])))

(module+ main
  (command-line
   #:program "public-sb-client"
   #:args (server port directory)
   (client-connect server (string->number port) directory)))
