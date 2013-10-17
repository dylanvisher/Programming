#lang scheme
(define SERVICE-PORT1 9010)
(define SERVICE-PORT2 9011)
(define SERVICE-PORT3 9012)
(define SERVER-HOST "localhost")

(define (loop1 y x)
  (cond
    [(= x 0) (loop1 y)]
    [else
     (local ((define y(number->string x)))
       (begin (system (string append 
               "wget http://10.1.50.1/image.jpg -O image"y".jpg ; convert image"y".jpg -rotate 180 -crop 1024x768-256+300 -crop 768x468+48+282 +repage"))
              (loop1 y (sub1 x))))]))
(define (serve1 port-no)
  (define listener (tcp-listen port-no 5 #t))
  (define (loop)
    (system "wget "http://10.1.50.1/image.jpg" -O /image${n}.jpg ; convert /var/ftp/pub/image${n}.jpg -rotate 180 -crop 1024x768-256+300 -crop 768x468+48+282 +repage "
    (accept-and-handle1 listener)
    (loop))
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener)))
(define server1 (serve1 SERVICE-PORT3))

(define (accept-and-handle1 listener)
  (define-values (client->me me->client) (tcp-accept listener))
  (handle1 client->me me->client)
  (close-input-port client->me)
  (close-output-port me->client))

(define (handle1 in out)
  (let ((input (read in)))
    (cond
      [(cons? input)
       (cond
         [(symbol=? (first input) 'player1)
          (cond
            [(symbol=? (second input) 'history)
             (write (player-history player1) out)])]
         [(symbol=? (first input) 'player2)
          (cond
            [(symbol=? (second input) 'history)
             (write (player-history player2) out)])])])))



(define (serve port-no1 port-no2)
  (define listener1 (tcp-listen port-no1 5 #t))
  (define listener2 (tcp-listen port-no2 5 #t))
  (define (loop)
    (accept-and-handle listener1 listener2)
    (loop))
  
  (define t (thread loop))
  (lambda ()
    (kill-thread t)
    (tcp-close listener1)
    (tcp-close listener2)))

(define server (serve SERVICE-PORT1 SERVICE-PORT2))


(define (accept-and-handle listener1 listener2)
  (define-values (client1->me me->client1) (tcp-accept listener1))
  (define-values (client2->me me->client2) (tcp-accept listener2))
  (handle client1->me me->client1 client2->me me->client2)
  (close-input-port client1->me)
  (close-input-port client2->me)
  (close-output-port me->client1)
  (close-output-port me->client2))


; handle : port port -> alop
(define (handle in1 out1 in2 out2)
  (let ((input1 (read in1))
        (input2 (read in2)))
    (cond
      [(and (symbol? input1)
            (symbol? input2))
       (cond
         [(and
           (or (symbol=? input1 'c)(symbol=? input1 'd))
           (or (symbol=? input2 'c)(symbol=? input2 'd))
           (local
               ((define new-players 
                  (add-to-score
                   (create-player input1 player1)
                   (create-player input2 player2)))
                (define p1 (first new-players))
                (define p2 (second new-players))
                (define p1-stats (cons 'player1 (player-stats p1)))
                (define p2-stats (cons 'player2 (player-stats p2))))             
             (and (write (list p1-stats p2-stats)out2)
                  (write (list p1-stats p2-stats) out1)))
           (and
            (write 'whoareyou out1)
            (write 'whoareyou out2)))])])))









