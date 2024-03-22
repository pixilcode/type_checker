#lang racket/base
(require racket/match
         racket/port
         racket/system
         json
         net/url
         file/sha1)

(define version "2024.winter.5")

(define (url path/query)
  (string->url (string-append "https://byu.kimball.germane.net" path/query)))

(define (read-key)
  (display "Enter key: ")
  (flush-output)
  (read-line (current-input-port) 'any))

(define-match-expander done
  (syntax-rules ()
    [(done message k-url)
     (hash-table ('tag "mt")
                 ('result-type "message")
                 ('message message)
                 ('k-url k-url))]))

(define-match-expander notice
  (syntax-rules ()
    [(notice message k-url)
     (hash-table ('tag "message")
                 ('message message)
                 ('k-url k-url))]))

(define-match-expander challenge
  (syntax-rules ()
    [(challenge payload k-url)
     (hash-table ('tag "challenge")
                 ('challenge payload)
                 ('k-url k-url))]))

(define (read-json* ip)
  (with-handlers ([exn:fail:read? (λ (e) #f)])
    (read-json ip)))

(module+ main
  (printf "grader version ~a\n" version)

  (define run-on-system
    (if (member (system-type) '(windows))
      (λ (command) (system* "ignore" 'exact command))
      system))
  
  (match (current-command-line-arguments)
    [(vector command-string)
     (match (read-json* (get-pure-port (url (bytes->string/utf-8 (hex-string->bytes (read-key))))))
       [#f
        (displayln "Session expired. Refresh the submission page.")]
       [(notice message k-url)
        (displayln message)
        (let loop ([port (get-pure-port (url k-url))])
          (match (read-json* port)
            [#f
             (displayln "Session expired. Refresh the submission page.")]
            [(done message k-url)
             (displayln message)
             (when k-url (printf "Continue submission in your web browser at ~a\n" (url->string (url k-url))))]
            [(notice message k-url)
             (displayln message)
             (loop (get-pure-port (url k-url)))]
            [(challenge payload k-url)
             (let ([os (open-output-string)])
               (if (parameterize ([current-input-port (open-input-string payload)]
                                  [current-output-port os])
                     (run-on-system command-string))
                 (let ([response (get-output-string os)])
                   (loop (post-pure-port
                          (url k-url)
                          (with-output-to-bytes (λ () (write-json (hasheq 'response response)))))))
                 (let ([response (get-output-string os)])
                   (begin
                     (displayln "Your implementation failed.")
                     (displayln "PROGRAM OUTPUT from your implementation")
                     (displayln response))
                   (loop (post-pure-port
                          (url k-url)
                          (with-output-to-bytes (λ () (write-json (hasheq 'response response)))))))))]))]
       [(done message k-url)
        (displayln message)
        (when k-url (printf "Continue submission in your web browser at ~a\n" (url->string (url k-url))))])]
    [(vector _ ...)
     (displayln #<<EOF
              usage: racket cs330-submit.rkt "<command to run artifact ...>"
example: racket cs330-local-test.rkt parser "acorn --ecma2020 | python3 banana.py"
EOF
                (current-error-port))]))
