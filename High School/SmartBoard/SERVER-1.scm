#lang scheme
(require scheme/system)
(define SERVICE-PORT 9001)
(define (command-string seconds)
  (string-append "wget  \"http://10.1.50.1/image.jpg" (make-string 1 #\") " -O " 
                 seconds".jpg ; convert "
                 seconds".jpg -rotate 180 -crop 1024x768-256+300 -crop 768x468+48+282 +repage "
                 seconds".png"))
(define (server)
  (let ([listener (tcp-listen SERVICE-PORT)])
    (let loop ()
      (let-values ([(client->me me->client)
                    (tcp-accept listener)]
                   [(secs)(number->string (current-seconds))])
        (system (command-string secs))
        (write secs me->client)
        (close-output-port me->client)
        (close-input-port client->me))
    (loop))))
(server)