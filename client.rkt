#lang racket/base
(require racket/cmdline
         racket/tcp
         racket/match
         racket/file
         racket/system
         racket/runtime-path)

(define-runtime-path log-file-path "sounds.log")
(unless (file-exists? log-file-path)
  (write-to-file (hash) log-file-path))

(define (sound-uses sound)
  (hash-ref (file->value log-file-path) sound 0))

(define (client-connect server port dir)
  (define-values (from to) (tcp-connect server port))
  (define sounds 
    (sort
     (sort (map path->string (directory-list dir))
           string-ci<?)
     >
     #:key sound-uses))

  (write sounds to)
  (flush-output to)

  (let loop ()
    (match (read from)
      [(? eof-object?)
       (error 'client "Server disconnected")]
      [(? string? some-sound)
       (when (member some-sound sounds)
         (write-to-file
          #:exists 'replace
          (hash-update (file->value log-file-path)
                       some-sound
                       add1
                       0)
          log-file-path)
         (define-values (sb stdout stdin stderr)
           (subprocess (current-output-port) (current-input-port) (current-error-port) 
                       "/usr/bin/mplayer"
                       "-quiet"
                       (build-path dir some-sound)))
         (void))
       (loop)])))

(module+ main
  (command-line
   #:program "public-sb-client"
   #:args (server port directory)
   (client-connect server (string->number port) directory)))
