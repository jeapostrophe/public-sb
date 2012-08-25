#lang racket/base
(require racket/cmdline
         racket/match
         racket/list
         racket/runtime-path
         racket/tcp
         racket/port
         web-server/http
         web-server/servlet-env
         web-server/dispatch)

(define-runtime-path root ".")

(define (every-n n f l)
  (cond
    [(empty? l)
     empty]
    [((length l) . < . n)
     (list (apply f l))]
    [else
     (define-values (these rest)
       (split-at l n))
     (cons (apply f these)
           (every-n n f rest))]))

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
        (title "Sound off!")
        (script
         "function sound(u) {
           var xmlhttp = new XMLHttpRequest();
           xmlhttp.open(\"GET\", u, true);
           xmlhttp.send();
          };"))
       (body
        ,(if to
           `(div
             (table
              ,@(every-n
                 3
                 (λ tds
                   `(tr ,@tds))
                 (for/list ([s (in-list sounds)])
                   `(td (a ([href ,(format "javascript:sound(~s);"
                                           (main-url page/play s))])
                           ,s)))))
             (p
              ,(format "The client is connected and ~a sounds are available."
                       (length sounds))))
           "The client is not connected.")))))

  (define (page/play req sound)
    (when to
      (write sound to)
      (flush-output to))
    (redirect-to (main-url page/main)))

  (define-values (main-dispatch main-url)
    (dispatch-rules
     [() page/main]
     [("") page/main]
     [("play" (string-arg)) page/play]))

  (serve/servlet
   main-dispatch
   #:listen-ip #f
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
