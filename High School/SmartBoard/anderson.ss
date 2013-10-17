
#lang scheme
(require rnrs/eval-6)
;(require (lib "1.ss" "srfi")
;         (lib "etc.ss"))
(require (only-in srfi/1 every))
(require (except-in scheme/gui/init
                    eval))
(require scheme/system)
(require (lib "process.ss"))
(require ffi/magick (for-syntax scheme/base))

;;;input and target sets for count-ones problem:
;(define-syntax (test stx)
;  (syntax-case stx ()
;    [(_ (func arg ...))
;     (with-syntax ([(tmp ...) (generate-temporaries #'(arg ...))])
;       #'(let ([tmp arg] ...)
;           (let ([r (func tmp ...)])
;             (printf "      -> ~s\n" r)
;             r)))]))

;; get-dimensions : string -> l
;(define (get-dimensions pic)
;  (list (test (MagickGetImageWidth pic))
;        (test (MagickGetImageHeight pic))))







;process-pic : pic -> list
(define (process-pic pic)
  (letrec (
           (bm (make-object bitmap% pic))
           (dc (new bitmap-dc% (bitmap bm)))
           (process-numbers 
            (lambda(pic) 
              (if (null? pic) '() 
                  (cons (* 1/255 (car pic)) (process-numbers (cdr pic))))))
           (getcolors 
            (lambda(pic) 
              (if (null? pic) '() 
                  (cons (cadddr pic) (getcolors (drop pic 4))))))
           ;(slide (test (MagickReadImage pic)))
           (dim '(24 15))
           (pix (apply * (cons 4 dim))))
    (let ((pixels (make-bytes pix)))
      (begin
        (send dc get-argb-pixels
              0 0 (car dim) (cadr dim)
              pixels)     
        (process-numbers (getcolors (bytes->list pixels)))))))


(define dataset (with-input-from-file "Input500new.txt" read))
(define input-set (map process-pic (map first dataset)))


;input-set
;input-set
"first"

;(caar input-set)
"second"
;(cadar input-set)



;sig : number -> number between 0 and 1
(define (sig total-input)
  (/ 1.0 (+ 1.0 (exp (- 0 total-input)))))

;(sig 1) 0.731058446378433
;(sig -8)
;(sig 7)


(define output-set (map (lambda(r) (map (lambda(x)(sig (/ (- x 10) 3))) r)) (map cadr dataset)))
;(
; (eval (caar input-set)
;       (environment '(rnrs))) (cadar input-set))
;process-row : listof(function args) -> output of function
(define (process-row r)
  ((eval (car r)
         (environment '(rnrs)))(cadr input-set)))
"process-row"
(process-row (first input-set))