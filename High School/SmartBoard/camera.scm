#lang scheme



  

 
  (define (picture->string fname)
    (local [(define in (open-input-file  fname))
            (define (read-loop fp input)
              (let ([byte (read-byte input)])
                (cond
                  [(eof-object? byte) (close-input-port input)]
                  (else (begin
                          (write-byte byte fp)
                          (read-loop fp input))))))]   
      (let ((output-string (open-output-string)))
        (begin
          (read-loop output-string in )
          (close-output-port output-string)
          output-string))))
 
  
    (define (picture->string1 fname)
    (local [(define in (open-input-file  fname))
            (define (read-loop  input)
              (let ([byte (read-byte input)])
                (cond
                  [(eof-object? byte) empty]
                  (else (cons
                           byte
                          (read-loop  input))))))]   
      (let ((output-string (read-loop in )))
          ;(close-output-port output-string)
          output-string)))
    
    
  (define (picture->file in-port fname)
    (local [(define in in-port)
            (define (read-loop fp input)
              (let ([byte (read-byte input)])
                (cond
                  [(eof-object? byte) (close-input-port input)]
                  (else (begin
                          (write-byte byte fp)
                          (read-loop fp input))))))]   
      (let ((fp (open-output-file fname 'replace)))
        (begin
          (read-loop fp in )
          (close-output-port fp)))))
  
  
  
(picture->string "1274893123.png")