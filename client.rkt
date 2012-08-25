#lang racket/base
(require racket/cmdline
         racket/tcp
         racket/match
         racket/system)

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
         (define-values (sb stdout stdin stderr)
           (subprocess #f #f #f 
                       "/usr/bin/mplayer"
                       (build-path dir some-sound)))
         (void))
       (loop)])))

(module+ main
  (command-line
   #:program "public-sb-client"
   #:args (server port directory)
   (client-connect server (string->number port) directory)))
