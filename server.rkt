#lang racket/base
(require racket/cmdline
         racket/runtime-path
         racket/tcp
         racket/port
         web-server/http
         web-server/servlet-env
         web-server/dispatch)

(define-runtime-path root ".")

(define (go http-port client-port)
  (define to #f)
  (define sounds #f)
  (thread
   (λ ()
     (define l (tcp-listen client-port 1 #t))
     (let accept-loop ()
       (define-values (from-p to-p) (tcp-accept l))
       (define sounds-in (read from-p))
       (cond
         [(and (list? sounds-in) (andmap string? sounds-in))
          (set! sounds sounds-in)
          (set! to to-p)
          (sync (eof-evt from-p))
          (set! to #f)
          (set! sounds #f)]
         [else
          (close-input-port from-p)])
       (close-output-port to-p)
       (accept-loop))))
  (define (page/main req)
    (response/xexpr
     `(html
       (head
        (title "Sound off!"))
       (body
        ,(if to
           `(div
             (p 
              ,(format "The client is connected and ~a sounds are available."
                       (length sounds)))
             (ul
              ,@(for/list ([s (in-list sounds)])
                  `(li
                    (a ([href ,(main-url page/play s)])
                       ,s)))))
           "The client is not connected.")))))

  (define (page/play req sound)
    (write sound to)
    (flush-output to)
    (redirect-to (main-url page/main)))

  (define-values (main-dispatch main-url)
    (dispatch-rules
     [() page/main]
     [("") page/main]
     [("play" (string-arg)) page/play]))

  (serve/servlet
   main-dispatch
   #:command-line? #t
   #:extra-files-paths
   (list (build-path root "static"))
   #:servlet-regexp #rx""
   #:port http-port))

(module+ main
  (command-line
   #:program "public-sb-server"
   #:args (http-port client-port)
   (go (string->number http-port)
       (string->number client-port))))
