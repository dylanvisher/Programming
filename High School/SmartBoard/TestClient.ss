#lang scheme
(require scheme/system)
(define SERVICE-PORT 9000)
(define SERVER-HOST "pc01.lab101")

  (define (client)
    (let-values ([(server->me me->server)
                  (tcp-connect SERVER-HOST SERVICE-PORT)])
      ;(write "10.png" me->server)
      (close-output-port me->server)
      (let ([response (read server->me)])
        (display response) (newline)
        (close-input-port server->me))))
(client)