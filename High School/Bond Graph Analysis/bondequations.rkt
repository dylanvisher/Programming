#lang racket

(provide (struct-out bond)
         (struct-out junction)
         (struct-out 1port)
         (struct-out 2port)
         (struct-out num)
         (struct-out var)
         (struct-out op)
         (struct-out deriv)
         (struct-out pvar)
         (struct-out svar)
         (struct-out dpvar)
         (struct-out posn)
	 display-posn
	 display-bond
	 display-1port
	 display-2port
	 display-junction
         display-bond-element
         graph->matrix)

(define display-bond-element
  (lambda(x)
    (cond
      [(num? x) (display-num x)]
      [(var? x) (display-var x)]
      [(op? x) (display-op x)]
      [(deriv? x) (display-deriv x)]
      [(pvar? x) (display-pvar x)]
      [(dpvar? x) (display-dpvar x)]
      [(svar? x) (display-svar x)]
      [(bond? x) (display-bond x)]
      [(junction? x) (display-junction x)]
      [(1port? x) (display-1port x)]
      [(2port? x) (display-2port x)]
      [(posn? x) (display-posn x)]
      [(list? x) (map display-bond-element x)]
      [(vector? x) (display-bond-element (vector->list x))]
      [else x])))

(define element=?
  (lambda(x y)
    (cond
      [(and (bond? x) (bond? y)) (bond=? x y)]
      [(and (junction? x) (junction? y)) (junction=? x y)]
      [(and (1port? x) (1port? y)) (1port=? x y)]
      [(and (2port? x) (2port? y)) (2port=? x y)]
      [else (equal? x y)])))
(define display-element
  (lambda(x)
    (cond
      [(bond? x) (display-bond x)]
      [(junction? x) (display-junction x)]
      [(1port? x) (display-1port x)]
      [(2port? x) (display-2port x)]
      [else x])))

(define-struct posn (x y))
(define display-posn (lambda(p) (list (posn-x p) (posn-y p))))

(define-struct bond (id in out causality) #:mutable)
(define display-bond (lambda(x) (list (bond-id x) (bond-in x) (bond-out x) (bond-causality x))))
(define bond=? (lambda(x y)
                 (and (= (bond-id x) (bond-id y))
                      (= (bond-in x) (bond-in y))
                      (= (bond-out x) (bond-out y))
                      (symbol=? (bond-causality x) (bond-causality y)))))

(define-struct junction (id type bonds) #:mutable)
(define display-junction (lambda(x) (list (junction-id x) (junction-type x) (junction-bonds x))))
(define junction=? (lambda(x y)
                     (and (= (junction-id x) (junction-id y))
                          (= (junction-type x) (junction-type y))
                          (equal? (junction-bonds x) (junction-bonds y)))))

(define-struct 1port (id type bond constant) #:mutable)
(define display-1port (lambda(x) (list (1port-id x) (1port-type x) (1port-bond x) (1port-constant x))))
(define 1port=? (lambda(x y)
                  (and (= (1port-id x) (1port-id y))
                       (symbol=? (1port-type x) (1port-type y))
                       (= (1port-bond x) (1port-bond y))
                       (= (1port-constant x) (1port-constant y)))))

(define-struct 2port (id type in out constant) #:mutable)
(define display-2port (lambda(x)
                        (list (2port-id x) (2port-type x) (2port-in x) (2port-out x) (2port-constant x))))
(define 2port=? (lambda(x y)
                  (and (= (2port-id x) (2port-id y))
                       (symbol=? (2port-type x) (2port-type y))
                       (= (2port-in x) (2port-in y))
                       (= (2port-out x) (2port-out x))
                       (= (2port-constant x) (2port-constant y)))))

(define-struct num (n))
(define display-num (lambda(x) (num-n x)))
(define num=? (lambda(x y) (= (num-n x) (num-n y))))

(define-struct var (s))
(define display-var (lambda(x) (var-s x)))
(define var=? (lambda(x y) (symbol=? (var-s x) (var-s y))))

(define-struct op (name left right) #:mutable)
(define display-op (lambda(x) (list (op-name x) (display-eqn (op-left x)) (display-eqn (op-right x)))))
(define op=? (lambda(x y) (and (symbol=? (op-name x) (op-name y))
                               (eqn=? (op-left x) (op-left y))
                               (eqn=? (op-right x) (op-right y)))))

(define-struct deriv (eqn))
(define display-deriv (lambda(x) (list 'deriv (display-eqn (deriv-eqn x)))))
(define deriv=? (lambda(x y) (eqn=? (deriv-eqn x) (deriv-eqn y))))

(define-struct pvar (type n))
(define display-pvar (lambda(x) (list (pvar-type x) (pvar-n x))))
(define pvar=? (lambda(x y) (and (symbol=? (pvar-type x) (pvar-type y))
                                 (= (pvar-n x) (pvar-n y)))))

(define-struct dpvar (type n dir))
(define display-dpvar (lambda(x) (list (dpvar-type x) (dpvar-n x) (dpvar-dir x))))
(define dpvar=? (lambda(x y) (and (symbol=? (dpvar-type x) (dpvar-type y))
                                  (= (dpvar-n x) (dpvar-n y))
                                  (symbol=? (dpvar-dir x) (dpvar-dir y)))))

(define-struct svar (type n))
(define display-svar (lambda(x) (list (svar-type x) (svar-n x))))
(define svar=? (lambda(x y) (and (symbol=? (svar-type x) (svar-type y))
                                 (= (svar-n x) (svar-n y)))))

(define display-eqn
  (lambda(x)
    (cond
      [(num? x) (display-num x)]
      [(var? x) (display-var x)]
      [(op? x) (display-op x)]
      [(deriv? x) (display-deriv x)]
      [(pvar? x) (display-pvar x)]
      [(dpvar? x) (display-dpvar x)]
      [(svar? x) (display-svar x)]
      [(list? x) (map display-eqn x)]
      [else x])))

(define eqn=?
  (lambda(x y)
    (cond
      [(and (num? x) (num? y)) (num=? x y)]
      [(and (var? x) (var? y)) (var=? x y)]
      [(and (op? x) (op? y)) (op=? x y)]
      [(and (deriv? x) (deriv? y)) (deriv=? x y)]
      [(and (pvar? x) (pvar? y)) (pvar=? x y)]
      [(and (dpvar? x) (dpvar? y)) (dpvar=? x y)]
      [(and (svar? x) (svar? y)) (svar=? x y)]
      [else false])))

; substitute: equation variable equation -> equation
; to replace all occurances of avar in aeqn with seqn
(define substitute
  (lambda(aeqn avar seqn)
    (cond
      [(num? aeqn) aeqn]
      [(deriv? aeqn)
       (make-deriv (substitute (deriv-eqn aeqn) avar seqn))]
      [(op? aeqn)
       (make-op (op-name aeqn)
                (substitute (op-left aeqn) avar seqn)
                (substitute (op-right aeqn) avar seqn))]
      [else
       (if (eqn=? aeqn avar) seqn aeqn)])))

(define element-id
  (lambda(x)
    (cond
      [(bond? x) (bond-id x)]
      [(1port? x) (1port-id x)]
      [(2port? x) (2port-id x)]
      [(junction? x) (junction-id x)]
      [else -1])))

(define element?
  (lambda(x)
    (or (bond? x) (1port? x) (2port? x) (junction? x))))

;sort-graph: graph -> graph
;sorts a graph in order of ascending id number, inserting an empty list for missing id numbers
(define sort-graph
  (lambda(agraph)
    (letrec((insert-spaces
             (lambda(bgraph counter)
               (cond
                 [(empty? bgraph) '()]
                 [(element? (car bgraph))
                  (cond
                    [(= (element-id (car bgraph)) counter)
                     (cons (car bgraph) (insert-spaces (cdr bgraph) (add1 counter)))]
                    [(> (element-id (car bgraph)) counter)
                     (insert-spaces bgraph (add1 counter))]
                    [else (insert-spaces bgraph (sub1 counter))])]
                 [else (insert-spaces (cdr bgraph) counter)]))))
      (insert-spaces (sort agraph #:key element-id <) 0))))

;duplicate-bond: bond -> bond
(define duplicate-bond
  (lambda(abond)
    (make-bond (bond-id abond)
               (bond-in abond)
               (bond-out abond)
               (bond-causality abond))))
;duplicate-1port: 1port->1port
(define duplicate-1port
  (lambda(a1port)
    (make-1port (1port-id a1port)
                (1port-type a1port)
                (1port-bond a1port)
                (1port-constant a1port))))
;duplicate-2port: 2port->2port
(define duplicate-2port
  (lambda(a2port)
    (make-2port (2port-id a2port)
                (2port-type a2port)
                (2port-in a2port)
                (2port-out a2port)
                (2port-constant a2port))))
;duplicate-junction: junction->junction
(define duplicate-junction
  (lambda(ajunct)
    (make-junction (junction-id ajunct)
                   (junction-type ajunct)
                   (build-list (length (junction-bonds ajunct))
                               (lambda(x) (list-ref (junction-bonds ajunct) x))))))
;duplicate: element->element
(define duplicate
  (lambda(aelement)
    (cond
      [(bond? aelement) (duplicate-bond aelement)]
      [(1port? aelement) (duplicate-1port aelement)]
      [(2port? aelement) (duplicate-2port aelement)]
      [(junction? aelement) (duplicate-junction aelement)]
      [else aelement])))
;duplicate-graph: graph->graph
(define duplicate-graph
  (lambda(agraph)
    (cond
      [(empty? agraph) '()]
      [else (cons (duplicate (car agraph))
                  (duplicate-graph (cdr agraph)))])))

;bondcheck: graph number -> graph
;replaces a bond with a properly directed bond
(define bondcheck
  (lambda(agraph n)
    (let ((abond (list-ref agraph n)))
      (cond
        [(bond? abond)
         (let* ((i (bond-in abond))
                (o (bond-out abond))               
                (in (list-ref agraph i))
                (out (list-ref agraph o))
                (check-in-1port-sink
                 (cond
                   [(1port? in)
                    (if (or (symbol=? 'R (1port-type in))
                            (symbol=? 'C (1port-type in))
                            (symbol=? 'L (1port-type in))) true false)]
                   [else false]))
                (check-out-1port-source
                 (cond
                   [(1port? out)
                    (if (or (symbol=? 'Se (1port-type out))
                            (symbol=? 'Sf (1port-type out))) true false)]
                   [else false]))
                (check-in-2port-in
                 (cond
                   [(2port? in)
                    (= (2port-in in) n)]
                   [else false]))
                (check-out-2port-out
                 (cond
                   [(2port? out)
                    (= (2port-out out) n)]
                   [else false])))
           (cond
             [(or check-in-1port-sink
                  check-out-1port-source
                  check-in-2port-in
                  check-out-2port-out)
              (begin (set-bond-in! abond o)
                     (set-bond-out! abond i)
                     (cond
                       [(symbol=? (bond-causality abond) 'effort)
                        (set-bond-causality! abond 'flow)]
                       [(symbol=? (bond-causality abond) 'flow)
                        (set-bond-causality! abond 'effort)])
                     agraph)]
             [else agraph]))]
        [else agraph]))))

;graphbondcheck: graph -> graph
;folds bondcheck on agraph
(define graphbondcheck
  (lambda(agraph)
    (let((bgraph (duplicate-graph agraph)))
      (foldr (lambda(x y) (bondcheck y x)) bgraph (build-list (length bgraph) (lambda(x) x))))))

;process-element: graph dpvar -> equation
;processes an element in graph agraph returning an equation for dpvar for later processing
(define process-element
  (lambda(agraph adpvar)
    (let* ((abond (list-ref agraph (dpvar-n adpvar)))
           (dir (dpvar-dir adpvar))
           (next (if (symbol=? 'up dir)
                     (list-ref agraph (bond-in abond))
                     (list-ref agraph (bond-out abond)))))       ;;
      (cond
        [(1port? next) (process-1port agraph next adpvar)]
        [(2port? next) (process-2port next adpvar)]
        [(junction? next) (process-junction agraph next adpvar)]
        [else 
         (display next) (newline)
         (error 'process-element "invalid element")]))))

;process-1port: graph 1port dpvar -> equation
;processes a 1port returning an equation for dpvar for later processing
(define process-1port
  (lambda(agraph a1port adpvar)
    (let ((type (1port-type a1port)))
      (cond
        [(symbol=? type 'R)  (process-R  a1port adpvar)]
        [(symbol=? type 'C)  (process-C  agraph a1port adpvar)]
        [(symbol=? type 'L)  (process-L  agraph a1port adpvar)]
        [(symbol=? type 'Sf) (process-Sf a1port adpvar)]
        [(symbol=? type 'Se) (process-Se a1port adpvar)]
        [else (error 'process-1port "invalid 1port" a1port)]))))

;process-R: 1port dpvar -> equation
;processes a type R 1port, returning an equation for dpvar for later processing
(define process-R
  (lambda(a1port adpvar)
    (cond
      [(symbol=? (dpvar-type adpvar) 'effort)
       (make-op 'mul
                (make-dpvar 'flow (dpvar-n adpvar)
                            (if (symbol=? (dpvar-dir adpvar) 'up)
                                'down 'up))
                (make-num (1port-constant a1port)))]
      [(symbol=? (dpvar-type adpvar) 'flow)
       (make-op 'div
                (make-dpvar 'effort (dpvar-n adpvar)
                            (if (symbol=? (dpvar-dir adpvar) 'up)
                                'down 'up))
                (make-num (1port-constant a1port)))])))

;process-C: graph 1port dpvar -> equation
;processes a type C 1port, returning an equation for dpvar for later processing
(define process-C
  (lambda(agraph a1port adpvar)
    (cond
      [(symbol=? (dpvar-type adpvar) 'effort)
       (if (integral? agraph a1port)
           (make-op 'div (make-svar 'q (1port-id a1port)) (make-num (1port-constant a1port)))
           (make-dpvar 'effort (dpvar-n adpvar) 'up))]
      [(symbol=? (dpvar-type adpvar) 'flow)
       (make-deriv (make-op 'mul
                            (make-dpvar 'effort (dpvar-n adpvar) 'up)
                            (make-num (1port-constant a1port))))])))

;process-L: graph 1port dpvar -> equation
;processes a type L 1port, returning an equation for dpvar for later processing
(define process-L
  (lambda(agraph a1port adpvar)
    (cond
      [(symbol=? (dpvar-type adpvar) 'flow)
       (if (integral? agraph a1port)
           (make-op 'div (make-svar 'p (1port-id a1port)) (make-num (1port-constant a1port)))
           (make-dpvar 'flow (dpvar-n adpvar) 'up))]
      [(symbol=? (dpvar-type adpvar) 'effort)
       (make-deriv (make-op 'mul
                            (make-dpvar 'flow (dpvar-n adpvar) 'up)
                            (make-num (1port-constant a1port))))])))

;process-Sf: 1port dpvar -> equation
;processes a type Sf 1port, returning an equation for dpvar for later processing
(define process-Sf
  (lambda(a1port adpvar)
    (cond
      [(symbol=? (dpvar-type adpvar) 'flow)
       (make-num (1port-constant a1port))]
      [(symbol=? (dpvar-type adpvar) 'effort)
       (make-dpvar 'effort (dpvar-n adpvar)
                   (if (symbol=? (dpvar-dir adpvar) 'up)
                       'down 'up))])))

;process-Se: 1port dpvar -> equation
;processesa type Se 1port, returning an equation for dpvar for later processing
(define process-Se
  (lambda(a1port adpvar)
    (cond
      [(symbol=? (dpvar-type adpvar) 'effort)
       (make-num (1port-constant a1port))]
      [(symbol=? (dpvar-type adpvar) 'flow)
       (make-dpvar 'flow (dpvar-n adpvar)
                   (if (symbol=? (dpvar-dir adpvar) 'up)
                       'down 'up))])))

;integral? : graph 1port -> boolean
;determines whether the 1port element is in integral or derivative causality
(define integral?
  (lambda(agraph a1port)
    (let*
        ((type (1port-type a1port))
         (bond-num (1port-bond a1port))
         (abond (list-ref agraph bond-num))
         (causality (bond-causality abond)))
      (cond
        [(symbol=? type 'R) true]
        [(symbol=? type 'Sf) (symbol=? causality 'flow)]
        [(symbol=? type 'Se) (symbol=? causality 'effort)]
        [(symbol=? type 'C) (symbol=? causality 'flow)]
        [(symbol=? type 'L) (symbol=? causality 'effort)]))))

;process-2port: 2port dpvar -> equation
;processes a 2port returning an equation for dpvar for later processing
(define process-2port
  (lambda(a2port adpvar)
    (cond
      [(symbol=? 'TF (2port-type a2port))
       (process-TF a2port adpvar)]
      [(symbol=? 'GY (2port-type a2port))
       (process-GY a2port adpvar)]
      [else (error 'process-2port "invalid 2port" a2port)])))

;process-TF: 2port dpvar -> equation
;processes a type TF 2port returning an equation for dpvar for later processing
(define process-TF
  (lambda(a2port adpvar)
    (let* ((dir
            (cond
              [(= (2port-in a2port) (dpvar-n adpvar)) 'forward]
              [else 'backward]))
           (next
            (cond
              [(symbol=? 'forward dir) (2port-out a2port)]
              [else (2port-in a2port)]))
           (type (dpvar-type adpvar)))
      (cond
        [(and (symbol=? 'forward dir) (symbol=? 'effort type))
         (make-op 'div (make-dpvar 'effort next (dpvar-dir adpvar)) (make-num (2port-constant a2port)))]
        [(and (symbol=? 'forward dir) (symbol=? 'flow type))
         (make-op 'mul (make-dpvar 'flow next (dpvar-dir adpvar)) (make-num (2port-constant a2port)))]
        [(and (symbol=? 'backward dir) (symbol=? 'effort type))
         (make-op 'mul (make-dpvar 'effort next (dpvar-dir adpvar)) (make-num (2port-constant a2port)))]
        [(and (symbol=? 'backward dir) (symbol=? 'flow type))
         (make-op 'div (make-dpvar 'flow next (dpvar-dir adpvar)) (make-num (2port-constant a2port)))]))))

;process-GY: 2port dpvar -> equation
;processes a type GY 2port returning an equation for dpvar for later processing
(define process-GY
  (lambda(a2port adpvar)
    (let* ((dir
            (cond
              [(= (2port-in a2port) (dpvar-n adpvar)) 'forward]
              [else 'backward]))
           (next
            (cond
              [(symbol=? 'forward dir) (2port-out a2port)]
              [else (2port-in a2port)]))
           (type (dpvar-type adpvar)))
      (cond
        [(and (symbol=? 'forward dir) (symbol=? 'effort type))
         (make-op 'mul (make-dpvar 'flow next (dpvar-dir adpvar)) (make-num (2port-constant a2port)))]
        [(and (symbol=? 'forward dir) (symbol=? 'flow type))
         (make-op 'div (make-dpvar 'effort next (dpvar-dir adpvar)) (make-num (2port-constant a2port)))]
        [(and (symbol=? 'backward dir) (symbol=? 'effort type))
         (make-op 'mul (make-dpvar 'flow next (dpvar-dir adpvar)) (make-num (2port-constant a2port)))]
        [(and (symbol=? 'backward dir) (symbol=? 'flow type))
         (make-op 'div (make-dpvar 'effort next (dpvar-dir adpvar)) (make-num (2port-constant a2port)))]))))

;causal-bond: graph junction -> number || symbol
;identifies the causal bond for a junction
;returns 'none if not yet assigned
(define causal-bond
  (lambda(agraph ajunct)
    (let*
        ((bonds (map (lambda(x) (list-ref agraph x)) (junction-bonds ajunct)))
         (bond-imposes
          (lambda(bond n)
            (cond
              [(= (bond-out bond) n) (bond-causality bond)]
              [else
               (cond
                 [(symbol=? (bond-causality bond) 'effort) 'flow]
                 [(symbol=? (bond-causality bond) 'flow) 'effort]
                 [else 'none])])))
         (test-bond
          (lambda(bond junct)
            (cond
              [(= (junction-type junct) 0)
               (if (symbol=? 'effort (bond-imposes bond (junction-id junct)))
                   (bond-id bond) false)]
              [(= (junction-type junct) 1)
               (if (symbol=? 'flow (bond-imposes bond (junction-id junct)))
                   (bond-id bond) false)])))
         (causal (filter number? (map (lambda(x) (test-bond x ajunct)) bonds))))
      (cond
        [(= (length causal) 1) (car causal)]
        [(= (length causal) 0) 'none]
        [else
         (error 'causal-bond (string-append
                              "improper causality on junction "
                              (number->string (junction-id ajunct))))]))))

;get-bonds: graph junction type -> (listof bond)
;gets all bonds connected to ajunct where ajunct receives (type is 'in) power from the bond
; or gives (type is 'out) power to the bond
(define get-bonds
  (lambda(agraph ajunct type)
    (let*
        ((bonds (map (lambda(x) (list-ref agraph x)) (junction-bonds ajunct)))
         (junct-id (junction-id ajunct))
         (filter-func
          (lambda(abond)
            (cond
              [(symbol=? type 'in) (= junct-id (bond-out abond))]
              [(symbol=? type 'out) (= junct-id (bond-in abond))]))))
      (map (lambda(x) (bond-id x)) (filter filter-func bonds)))))

;process-junction: graph junction dpvar -> equation
;processes a junction returning an equation for dpvar for later processing
(define process-junction
  (lambda(agraph ajunct adpvar)
    (letrec
        ((c-bond (causal-bond agraph ajunct))
         (all-in-bonds (get-bonds agraph ajunct 'in))
         (all-out-bonds (get-bonds agraph ajunct 'out))
         (this-bond (dpvar-n adpvar))
         (this-bond-type
          (if (= (bond-out (list-ref agraph (dpvar-n adpvar))) (junction-id ajunct)) 'in 'out))
         (in-bonds (filter (lambda(x) (not (= x this-bond))) all-in-bonds))
         (out-bonds (filter (lambda(x) (not (= x this-bond))) all-out-bonds))
         (bond-dpvar
          (lambda(abond)
            (make-dpvar (dpvar-type adpvar) (bond-id abond)
                        (if (= (junction-id ajunct) (bond-out abond))
                            'up 'down))))
         (add-all
          (lambda(dpvars)
            (cond
              [(null? dpvars) (make-num 0)]
              [(null? (cdr dpvars)) (car dpvars)]
              [else (make-op 'add (car dpvars) (add-all (cdr dpvars)))])))
         (in-bond-sum (add-all (map (lambda(x) (bond-dpvar (list-ref agraph x))) in-bonds)))
         (out-bond-sum (add-all (map (lambda(x) (bond-dpvar (list-ref agraph x))) out-bonds))))
      (cond
        [(symbol=?
          (if (= (junction-type ajunct) 0) 'effort 'flow)
          (dpvar-type adpvar))
         (bond-dpvar (list-ref agraph c-bond))]
        [(symbol=? this-bond-type 'in)
         (cond
           [(null? in-bond-sum) out-bond-sum]
           [(null? out-bond-sum) (make-op 'mul (make-num -1) in-bond-sum)]
           [else (make-op 'sub out-bond-sum in-bond-sum)])]
        [else
         (cond
           [(null? in-bond-sum) (make-op 'mul (make-num -1) out-bond-sum)]
           [(null? out-bond-sum) in-bond-sum]
           [else (make-op 'sub in-bond-sum out-bond-sum)])]))))

;stepper: graph equation -> equation
;takes a graph and an equation and steps through it until completion
(define stepper
  (lambda(agraph aeqn)
    ((letrec
         ((s
           (lambda(bgraph beqn dpvars)
             (cond
               [(or (num? beqn) (var? beqn) (pvar? beqn) (svar? beqn)) beqn]
               [(dpvar? beqn)
                (if (foldr (lambda(x y) (or x y)) false (map (lambda(x) (dpvar=? beqn x)) dpvars))
                    (pvar (dpvar-type beqn) (dpvar-n beqn))
                    (s bgraph (process-element bgraph beqn) (cons beqn dpvars)))]
               [(op? beqn)
                (make-op (op-name beqn)
                         (s bgraph (op-left beqn) dpvars)
                         (s bgraph (op-right beqn) dpvars))]
               [(deriv? beqn)
                (make-deriv (s bgraph (deriv-eqn beqn) dpvars))]
               [else
                (newline) (display "ERROR") (newline)
                (begin (map (lambda(x) (display (display-element x)) (newline)) agraph) (newline) (newline))
                (display (display-eqn beqn)) (newline)
                (error 'stepper "invalid equation type")]))))
       s) agraph aeqn '())))

;take-deriv : graph equation -> equation
;takes the time derivative of the equation
;all vars and pvars are left alone
;used when wrong pvar requested from a C or L
(define take-deriv
  (lambda(agraph aeqn)
    (cond
      [(num? aeqn) (make-num 0)]
      [(var? aeqn) aeqn]
      [(op? aeqn)
       (cond
         [(or (symbol=? 'add (op-name aeqn))
              (symbol=? 'sub (op-name aeqn)))
          (make-op (op-name aeqn) (take-deriv agraph (op-left aeqn)) (take-deriv agraph (op-right aeqn)))]
         [(symbol=? 'mul (op-name aeqn))
          (product-rule agraph aeqn)]
         [(symbol=? 'div (op-name aeqn))
          (quotient-rule agraph aeqn)]
         [else (error 'take-deriv (string-append "invalid op-type in "
                                                 (list->string (display-eqn aeqn))))])]
      [(deriv? aeqn)
       (take-deriv agraph (deriv-eqn aeqn))]
      [(pvar? aeqn) aeqn]
      [(dpvar? aeqn)
       (error 'take-deriv "cannot contain dpvars")]
      [(svar? aeqn)
       (cond
         [(symbol=? 'p (svar-type aeqn)) (make-pvar 'effort
                                                    (bond-id
                                                     (list-ref agraph
                                                               (1port-bond
                                                                (list-ref agraph (svar-n aeqn))))))]
         [else (make-pvar 'flow
                          (bond-id
                           (list-ref agraph
                                     (1port-bond
                                      (list-ref agraph (svar-n aeqn))))))])])))

;product-rule : graph equation -> equation
;takes in a mul-type op structure and applies the product rule of calculus to it
(define product-rule
  (lambda(agraph aeqn)
    (make-op 'add
             (make-op 'mul (op-left aeqn) (take-deriv agraph (op-right aeqn)))
             (make-op 'mul (take-deriv agraph (op-left aeqn)) (op-right aeqn)))))

;quotient-rule : graph equation -> equation
;takes in a div-type op structure and applies the quotient rule of calculus to it
(define quotient-rule
  (lambda(agraph aeqn)
    (make-op 'div
             (make-op 'sub
                      (make-op 'mul
                               (take-deriv agraph (op-left aeqn))
                               (op-right aeqn))
                      (make-op 'mul
                               (op-left aeqn)
                               (take-deriv agraph (op-right aeqn))))
             (make-op 'mul
                      (op-right aeqn)
                      (op-right aeqn)))))

;evaluate-derivs : graph equation -> equation
;processes an equation and symbolically takes any derivatives it contains
(define evaluate-derivs
  (lambda(agraph aeqn)
    (cond
      [(or (num? aeqn) (var? aeqn) (pvar? aeqn) (svar? aeqn))
       aeqn]
      [(dpvar? aeqn)
       (error 'evaluate-derivs (string-append "cannot contain dpvars: " (list->string (display-eqn aeqn))))]
      [(op? aeqn)
       (make-op (op-name aeqn)
                (evaluate-derivs agraph (op-left aeqn))
                (evaluate-derivs agraph (op-right aeqn)))]
      [(deriv? aeqn)
       (take-deriv agraph aeqn)])))

;get-integral-storage: graph -> (listof num)
;gets the id numbers of all storage (C or L) elements in the graph in integral causality
(define get-integral-storage
  (lambda(agraph)
    (map
     (lambda(x) (1port-id x))
     (filter (lambda(x) (if (1port? x)
                            (and (integral? agraph x)
                                 (or (symbol=? (1port-type x) 'L)
                                     (symbol=? (1port-type x) 'C)))
                            false))
             agraph))))

;get-integral-eqns: graph (listof num) -> (listof eqn)
;takes a graph and a list of id numbers of integral 1port storage elements (created by get-integral-storage)
;and creates an equation equating the rate of change of its storage variable to a dpvar
(define get-integral-eqns
  (lambda(agraph nums)
    (let
        ((get-eqn
          (lambda(anum)
            (let*
                ((aelement (list-ref agraph anum))
                 (type (1port-type aelement))
                 (bond (1port-bond aelement)))
              (make-op 'equals
                       (make-pvar (if (symbol=? type 'L) 'effort 'flow) bond)
                       (make-dpvar (if (symbol=? type 'L) 'effort 'flow) bond 'up))))))
      (map get-eqn nums))))

;1stpass : graph -> (listof eqn)
;1st pass through a graph
;gets the integral storage elements, creates equations, steps through, and evaluates derivatives
(define 1stpass
  (lambda(agraph)
    (let*
        ((ielements (get-integral-storage agraph))
         (initialequations (get-integral-eqns agraph ielements))
         (steppedequations (map (lambda(x) (stepper agraph x)) initialequations))
         (derivedequations (map (lambda(x) (evaluate-derivs agraph x)) steppedequations)))
      derivedequations)))

;valid-variable: graph 1port -> (listof pvar svar)
;takes a 1port element and lists the valid variables that can exist from it without auxiliary functions
(define valid-variable
  (lambda(agraph a1port)
    (let
        ((type (1port-type a1port))
         (id (1port-id a1port))
         (bond (1port-bond a1port)))
      (cond
        [(symbol=? type 'Sf) (list (make-pvar 'flow bond))]
        [(symbol=? type 'Se) (list (make-pvar 'effort bond))]
        [(symbol=? type 'C)
         (if (integral? agraph a1port)
             (list (make-pvar 'flow bond)
                   (make-svar 'q id))
             '())]
        [(symbol=? type 'L)
         (if (integral? agraph a1port)
             (list (make-pvar 'effort bond)
                   (make-svar 'p id))
             '())]
        [else '()]))))

;valid-variables: graph -> (listof pvar svar)
;lists all valid variables from a graph that do not need auxiliary functions
(define valid-variables
  (lambda(agraph)
    (apply append (map (lambda(x) (if (1port? x) (valid-variable agraph x) '())) agraph))))

;check-eqn: equation (listof pvar svar) -> (listof pvar svar)
;lists all pvars and svars in aeqn that do not occur in valid (and thus require an auxiliary function
(define check-eqn
  (lambda(aeqn valid)
    (cond
      [(or (num? aeqn) (var? aeqn)) empty]
      [(or (pvar? aeqn) (svar? aeqn))
       (if (foldr (lambda(x y) (or x y)) false (map (lambda(x) (eqn=? aeqn x)) valid))
           empty
           (list aeqn))]
      [(op? aeqn)
       (append (check-eqn (op-left aeqn) valid)
               (check-eqn (op-right aeqn) valid))]
      [(deriv? aeqn)
       (check-eqn (deriv-eqn aeqn) valid)])))

;get-aux-vars : (listof equation) (listof pvar svar) -> (listof pvar svar)
;gets all necessary auxiliary variables from a graph
(define get-aux-vars
  (lambda(eqns valid)
    (apply append (map (lambda(x) (check-eqn x valid)) eqns))))

;aux-func : graph (or pvar svar) -> equation
;creates an auxiliary function for a variable in a graph
(define aux-func
  (lambda(agraph avar)
    (make-op 'equals avar
             (stepper agraph
                      (cond
                        [(pvar? avar)
                         (let((abond (list-ref agraph (pvar-n avar))))
                           (make-dpvar (pvar-type avar) (pvar-n avar)
                                       (if (symbol=? (pvar-type avar) (bond-causality abond))
                                           'up 'down)))]
                        [(svar? avar)
                         (let((a1port (list-ref agraph (svar-n avar))))
                           (if (symbol=? 'C (1port-type a1port))
                               (make-op 'mul
                                        (make-dpvar 'effort (1port-bond a1port) 'up)
                                        (1port-constant a1port))
                               (make-op 'mul
                                        (make-dpvar 'flow (1port-bond a1port) 'up)
                                        (1port-constant a1port))))])))))

;2ndpass : graph -> (list (listof eqn) (listof eqn))
;turns the graph into equations, divided between differential and auxiliary
(define 2ndpass
  (lambda(agraph)
    (letrec
        (
         (pass1 (list (1stpass agraph) '()))
         (valid (valid-variables agraph))
         ;make-pass2: graph (list (listof eqn) (listof eqn)) -> (list (listof eqn) (listof eqn))
         (make-pass2
          (lambda(agraph eqns)
            (let*
                (
                 (eqn-vars (map op-left (cadr eqns)))
                 (new-valid (append valid eqn-vars))
                 (needed-auxs (get-aux-vars (apply append eqns) new-valid))
                 )
              (if (null? needed-auxs)
                  eqns
                  (make-pass2 agraph
                              (list (car eqns)
                                    (append (cadr eqns)
                                            (map (lambda(x) (aux-func agraph x)) needed-auxs)))))))))
      (make-pass2 agraph pass1))))

;element-of: list number -> number
;returns whether n is on alist
(define element-of
  (lambda(alist n)
    (cond
      [(empty? alist) false]
      [(= n (car alist)) true]
      [else (element-of (cdr alist) n)])))

;junct-causality: vgraph number -> vgraph
;propogates causality on junction n
(define junct-causality
  (lambda(agraph n)
    (let*
        ((this (vector-ref agraph n))
         (bonds (junction-bonds this))
         (causal-bond (causal-bond (vector->list agraph) this))
         (assigned-bonds
          (filter (lambda(x) (or (symbol=? 'effort (bond-causality (vector-ref agraph x)))
                                 (symbol=? 'flow (bond-causality (vector-ref agraph x)))))
                  bonds)))
      (cond
        [(number? causal-bond)
         (map
          (lambda(x)
            (vector-set! agraph x
                         (if (not (= x causal-bond))
                             (let ((y (vector-ref agraph x)))
                               (make-bond
                                (bond-id y) (bond-in y) (bond-out y)
                                (if
                                 (= n (bond-in y))
                                 (if
                                  (= 0 (junction-type this))
                                  'effort 'flow)
                                 (if
                                  (= 0 (junction-type this))
                                  'flow 'effort))))
                             (vector-ref agraph x))))
          bonds)]
        [(= (sub1 (length bonds)) (length assigned-bonds))
         (map
          (lambda(x)
            (vector-set! agraph x
                         (if (not (element-of assigned-bonds x))
                             (let ((y (vector-ref agraph x)))
                               (make-bond
                                (bond-id y) (bond-in y) (bond-out y)
                                (if
                                 (= n (bond-in y))
                                 (if
                                  (= 0 (junction-type this))
                                  'flow 'effort)
                                 (if
                                  (= 0 (junction-type this))
                                  'effort 'flow))))
                             (vector-ref agraph x))))
          bonds)]
        [else false])
      agraph)))

;2port-causality: vgraph number -> vgraph
;propagates causality across a 2port
(define 2port-causality
  (lambda(agraph n)
    (let*
        (
         (bgraph (build-vector (vector-length agraph)
                               (lambda(x)
                                 (apply 
                                  (cond
                                    [(bond? (vector-ref agraph x)) make-bond]
                                    [(1port? (vector-ref agraph x)) make-1port]
                                    [(2port? (vector-ref agraph x)) make-2port]
                                    [(junction? (vector-ref agraph x)) make-junction])
                                  (display-element (vector-ref agraph x))))))
         (this (vector-ref bgraph n))
         (type (2port-type this))
         (this-in (2port-in this))
         (in-causality (bond-causality (vector-ref bgraph this-in)))
         (this-out (2port-out this))
         (out-causality (bond-causality (vector-ref bgraph this-out))))
      (cond
        [(and (or (symbol=? 'effort in-causality)
                  (symbol=? 'flow in-causality))
              (not (or (symbol=? 'effort out-causality)
                       (symbol=? 'flow out-causality))))
         (set-bond-causality! (vector-ref bgraph this-out)
                              (if (symbol=? type 'TF) in-causality
                                  (cond
                                    [(symbol=? 'effort in-causality) 'flow]
                                    [(symbol=? 'flow in-causality) 'effort]
                                    [else 'none])))]
        [(and (or (symbol=? 'effort out-causality)
                  (symbol=? 'flow out-causality))
              (not (or (symbol=? 'effort in-causality)
                       (symbol=? 'flow in-causality))))
         (set-bond-causality! (vector-ref bgraph this-in)
                              (if (symbol=? type 'TF) out-causality
                                  (cond
                                    [(symbol=? 'effort out-causality) 'flow]
                                    [(symbol=? 'flow out-causality) 'effort]
                                    [else 'none])))]
        [else void])
      bgraph)))

;element-causality: vgraph number -> vgraph
;propagates causality across any element
(define element-causality
  (lambda(agraph n)
    (let((element (vector-ref agraph n)))
      (cond
        [(junction? element) (junct-causality agraph n)]
        [(2port? element) (2port-causality agraph n)]
        [else agraph]))))

;causal-step: vgraph -> vgraph
;folds element-causality across vgraph
(define causal-step
  (lambda(agraph)
    (letrec
        ((bgraph (foldl (lambda(n ag) (element-causality ag n))
                        agraph
                        (build-list (vector-length agraph) (lambda(x) x))))
         (vgraph=?
          (lambda(g1 g2 . n)
            (let ((x (if (cons? n) (car n) 0)))
              (if (= (vector-length g1) (vector-length g2))
                  (cond
                    [(>= x (vector-length g1)) true]
                    [(element=? (vector-ref g1 x) (vector-ref g2 x))
                     (vgraph=? g1 g2 (add1 x))]
                    [else false])
                  false)))))
      (if (vgraph=? agraph bgraph) agraph (causal-step bgraph)))))

;new-causality: vgraph number -> vgraph
;takes in a graph and a number of the element for which causality should be set
;and sets causality correspondingly
(define new-causality
  (lambda(agraph n)
    (let*
        ((bgraph (build-vector (vector-length agraph)
                               (lambda(x)
                                 (apply 
                                  (cond
                                    [(bond? (vector-ref agraph x)) make-bond]
                                    [(1port? (vector-ref agraph x)) make-1port]
                                    [(2port? (vector-ref agraph x)) make-2port]
                                    [(junction? (vector-ref agraph x)) make-junction])
                                  (display-element (vector-ref agraph x))))))
         (this (vector-ref bgraph n)))
      (cond
        [(bond? this)
         (set-bond-causality! this 'effort)]
        [(1port? this)
         (let
             ((type (1port-type this)))
           (cond
             [(or (symbol=? type 'R) (symbol=? type 'L) (symbol=? type 'Se))
              (set-bond-causality! (vector-ref bgraph (1port-bond this)) 'effort)]
             [(or (symbol=? type 'C) (symbol=? type 'Sf))
              (set-bond-causality! (vector-ref bgraph (1port-bond this)) 'flow)]
             [else void]))]
        [else void])
      bgraph)))

;causal?: vgraph number -> boolean
;determines whether element n has causality assign
;always returns true for junctions and 2ports
(define causal?
  (lambda(agraph n)
    (let ((this (vector-ref agraph n)))
      (cond
        [(bond? this)
         (or (symbol=? (bond-causality this) 'effort)
             (symbol=? (bond-causality this) 'flow))]
        [(1port? this)
         (or (symbol=? (bond-causality (vector-ref agraph (1port-bond this))) 'effort)
             (symbol=? (bond-causality (vector-ref agraph (1port-bond this))) 'flow))]
        [else true]))))

;next-causal-element: vgraph -> number || false
;determines the next element to have causality assigned
;returns false
(define next-causal-element
  (lambda(agraph)
    (let*
        ((uncausal 
          (map (lambda(z) (vector-ref agraph (cadr z)))
               (filter (lambda(y) (not (car y)))
                       (map (lambda(x) (list (causal? agraph x) x))
                            (build-list (vector-length agraph) (lambda(x) x))))))
         (uncausal-sources
          (map 1port-id
               (filter (lambda(x)
                         (cond
                           [(1port? x) (or (symbol=? 'Sf (1port-type x))
                                           (symbol=? 'Se (1port-type x)))]
                           [else false]))
                       uncausal)))
         (uncausal-storage
          (map 1port-id
               (filter (lambda(x)
                         (cond
                           [(1port? x) (or (symbol=? 'C (1port-type x))
                                           (symbol=? 'L (1port-type x)))]
                           [else false]))
                       uncausal)))
         (uncausal-resistance
          (map 1port-id
               (filter (lambda(x)
                         (cond
                           [(1port? x) (symbol=? 'R (1port-type x))]
                           [else false]))
                       uncausal)))
         (uncausal-bonds
          (map bond-id (filter bond? uncausal))))
      (cond
        [(empty? uncausal) false]
        [(cons? uncausal-sources) (car uncausal-sources)]
        [(cons? uncausal-storage) (car uncausal-storage)]
        [(cons? uncausal-resistance) (car uncausal-resistance)]
        [(cons? uncausal-bonds) (car uncausal-bonds)]
        [else false]))))

;causality: vgraph -> vgraph
;takes in a graph and propagates causality
(define causality
  (lambda(agraph)
    (letrec
        ((bgraph (causal-step agraph))
         (next (next-causal-element bgraph)))
      (cond
        [(boolean? next) bgraph]
        [else (causality (new-causality bgraph next))]))))

;graph->eqns : graph -> equations
;takes in a list graph, converts to a vector, applies causality
; converts back to a list, applies 2ndpass, and applies append
(define graph->eqns
  (lambda(agraph)
    (apply append (2ndpass (vector->list (causality (list->vector agraph)))))))

;get-variables : graph (listof eqn) -> (listof (list (svar OR pvar) var))
;takes a graph and a list of equations and produces a list of variables to substitute in
(define get-variables
  (lambda(agraph eqns)
    (letrec
        ((change-to-var
          (lambda(apvar)
            (cond
              [(pvar? apvar)
               (cond
                 [(1port? (list-ref agraph (bond-out (list-ref agraph (pvar-n apvar)))))
                  (cond
                    [(and (symbol=? (pvar-type apvar) 'flow)
                          (symbol=? 'C (1port-type (list-ref agraph (bond-out (list-ref agraph (pvar-n apvar)))))))
                     (make-var (string->symbol (string-append "q"
                                                              (number->string
                                                               (1port-id
                                                                (list-ref agraph
                                                                          (bond-out (list-ref agraph
                                                                                              (pvar-n apvar))))))
                                                              "dot")))]
                    [(and (symbol=? (pvar-type apvar) 'effort)
                          (symbol=? 'L (1port-type (list-ref agraph (bond-out (list-ref agraph (pvar-n apvar)))))))
                     (make-var (string->symbol (string-append "p"
                                                              (number->string
                                                               (1port-id
                                                                (list-ref agraph
                                                                          (bond-out (list-ref agraph
                                                                                              (pvar-n apvar))))))
                                                              "dot")))]
                    [else
                     (make-var (string->symbol
                                (string-append
                                 (symbol->string (pvar-type apvar))
                                 (number->string (pvar-n apvar)))))])]
                 [else
                  (make-var (string->symbol
                             (string-append
                              (symbol->string (pvar-type apvar))
                              (number->string (pvar-n apvar)))))])]
              [(svar? apvar)
               (make-var (string->symbol
                          (string-append
                           (symbol->string (svar-type apvar))
                           (number->string (svar-n apvar)))))]
              [else apvar])))
         (get-vars
          (lambda(x)
            (cond
              [(or (svar? x) (pvar? x)) (list x)]
              [(op? x) (append (get-vars (op-left x)) (get-vars (op-right x)))]
              [(deriv? x) (get-vars (deriv-eqn x))]
              [else '()])))
         (remove-duplicates
          (lambda(alist)
            (cond
              [(empty? alist) '()]
              [(foldr (lambda(x y) (or x y)) false (map (lambda(x) (eqn=? (car alist) x)) (cdr alist)))
               (remove-duplicates (cdr alist))]
              [else (cons (car alist) (remove-duplicates (cdr alist)))])))
         (vars (remove-duplicates (apply append (map get-vars eqns)))))
      (map (lambda(x) (list x (change-to-var x))) vars))))

;substitute-all: graph (listof eqn) -> (listof eqn)
;applies get-variables, then performs all substitutions indicated indicates
(define substitute-all
  (lambda(agraph eqns)
    (let
        ((varlist (get-variables agraph eqns)))
      (map
       (lambda(aeqn)
         (foldl
          (lambda(avar beqn)
            (substitute beqn (car avar) (cadr avar)))
          aeqn
          varlist))
       eqns))))

;distribute: eqn -> eqn
;uses the distributive property on an equation
(define distribute
  (lambda(aeqn)
    (cond
      [(not (op? aeqn)) aeqn]
      [else
       (cond
         [(or (symbol=? 'mul (op-name aeqn))
              (symbol=? 'div (op-name aeqn)))
          (cond
            [(and (or (num? (op-left aeqn)) (var? (op-left aeqn)))
                  (op? (op-right aeqn)))
             (cond
               [(or (symbol=? 'add (op-name (op-right aeqn)))
                    (symbol=? 'sub (op-name (op-right aeqn))))
                (make-op (op-name (op-right aeqn))
                         (make-op (op-name aeqn)
                                  (op-left aeqn)
                                  (op-left (op-right aeqn)))
                         (make-op (op-name aeqn)
                                  (op-left aeqn)
                                  (op-right (op-right aeqn))))]
               [else (make-op (op-name aeqn)
                              (distribute (op-left aeqn))
                              (distribute (op-right aeqn)))])]
            [(and (op? (op-left aeqn))
                  (or (num? (op-right aeqn)) (var? (op-right aeqn))))
             (cond
               [(or (symbol=? 'add (op-name (op-left aeqn)))
                    (symbol=? 'sub (op-name (op-left aeqn))))
                (make-op (op-name (op-left aeqn))
                         (make-op (op-name aeqn)
                                  (op-left (op-left aeqn))
                                  (op-right aeqn))
                         (make-op (op-name aeqn)
                                  (op-right (op-left aeqn))
                                  (op-right aeqn)))]
               [else (make-op (op-name aeqn)
                              (distribute (op-left aeqn))
                              (distribute (op-right aeqn)))])]
            [(and (symbol=? 'div (op-name aeqn))
                  (op? (op-left aeqn)))
             (if (or (symbol=? 'add (op-name (op-left aeqn)))
                     (symbol=? 'sub (op-name (op-left aeqn))))
                 (make-op (op-name (op-left aeqn))
                          (make-op 'div (op-left (op-left aeqn)) (op-right aeqn))
                          (make-op 'div (op-right (op-left aeqn)) (op-right aeqn)))
                 (make-op (op-name aeqn)
                          (distribute (op-left aeqn))
                          (distribute (op-right aeqn))))]
            [else (make-op (op-name aeqn)
                           (distribute (op-left aeqn))
                           (distribute (op-right aeqn)))])]
         [else (make-op (op-name aeqn)
                        (distribute (op-left aeqn))
                        (distribute (op-right aeqn)))])])))

;remove-subs: eqn -> eqn
;replaces all type sub ops in eqn with (op 'add op-left (op 'mul op-right (num -1))
(define remove-subs
  (lambda(aeqn)
    (cond
      [(op? aeqn)
       (cond
         [(symbol=? 'sub (op-name aeqn))
          (make-op 'add
                   (remove-subs (op-left aeqn))
                   (make-op 'mul (remove-subs (op-right aeqn)) (make-num -1)))]
         [else (make-op (op-name aeqn)
                        (remove-subs (op-left aeqn))
                        (remove-subs (op-right aeqn)))])]
      [(deriv? aeqn)
       (make-deriv (remove-subs (deriv-eqn aeqn)))]
      [else aeqn])))

;flatten-eqn: eqn -> eqn
;applies remove-subs, then recurses through distribute until the equation doesn't change
(define flatten-eqn
  (lambda(aeqn)
    (letrec
        ((beqn (remove-subs aeqn))
         (fl-eqn
          (lambda(ceqn)
            (let
                ((deqn (distribute ceqn)))
              (if (eqn=? ceqn deqn) ceqn (fl-eqn deqn))))))
      (fl-eqn beqn))))

;eqn->list: eqn -> (listof eqn)
;converts a nested type add op eqn into a list of eqns
(define eqn->list
  (lambda(aeqn)
    (cond
      [(op? aeqn)
       (cond
         [(symbol=? 'add (op-name aeqn))
          (append (eqn->list (op-left aeqn))
                  (eqn->list (op-right aeqn)))]
         [else (list aeqn)])]
      [else (list aeqn)])))

;extract-var: eqn -> var
;extracts the variable of a nested type mul or div eqn
(define extract-var
  (lambda(aeqn)
    (cond
      [(var? aeqn) aeqn]
      [(num? aeqn) false]
      [else
       (if (boolean? (extract-var (op-left aeqn)))
           (extract-var (op-right aeqn))
           (extract-var (op-left aeqn)))])))

;eqn->num: eqn -> number
;converts an equation to numbers
(define eqn->num
  (lambda(aeqn)
    (cond
      [(num? aeqn) (num-n aeqn)]
      [(op? aeqn)
       (let ((l (eqn->num (op-left aeqn)))
             (r (eqn->num (op-right aeqn))))
         (if (and (number? l) (number? r))
             ((let ((o (op-name aeqn)))
                (cond
                  [(symbol=? o 'add) +]
                  [(symbol=? o 'sub) -]
                  [(symbol=? o 'mul) *]
                  [(symbol=? o 'div) /]))
              l r)
             (make-op (op-name aeqn) l r)))]
      [else aeqn])))

;extract-coefficient: eqn -> num
;extracts the coefficient of the variable in an equation
(define extract-coefficient
  (lambda(aeqn)
    (let ((avar (extract-var aeqn)))
      (if (not (boolean? avar))
          (make-num (eqn->num (substitute aeqn (extract-var aeqn) (make-num 1))))
          (make-num (eqn->num aeqn))))))

;simplify-term: eqn -> eqn
;simplifies a term into the product of a coefficient and variable
(define simplify-term
  (lambda(aeqn)
    (let ((avar (extract-var aeqn))
          (acoef (extract-coefficient aeqn)))
      (if (not (boolean? avar))
          (make-op 'mul acoef avar)
          acoef))))

;simplify-eqn: eqn -> (listof eqn)
;converts an equation into a list of equations, each of which is (op 'mul (num n) (var v))
(define simplify-eqn
  (lambda(aeqn)
    (cond
      [(op? aeqn)
       (cond
         [(symbol=? (op-name aeqn) 'equals)
          (list (op-left aeqn)
                (map simplify-term (eqn->list (flatten-eqn (op-right aeqn)))))]
         [else (map simplify-term (eqn->list (flatten-eqn aeqn)))])]
      [else (map simplify-term (eqn->list (flatten-eqn aeqn)))])))

;get-mvars: graph (listof eqns) symbol -> (listof var)
;extracts the variables in eqns for either matrix 'X or 'U
(define get-mvars
  (lambda(agraph eqns which)
    (letrec((vars (map cadr (get-variables agraph eqns)))
            (pass1 (filter
                    (lambda(x)
                      (cond
                        [(symbol=? 'X which)
                         (or (not (boolean? (regexp-match (regexp "dot") (symbol->string (var-s x)))))
                             (and (boolean? (regexp-match (regexp "p") (symbol->string (var-s x))))
                                  (boolean? (regexp-match (regexp "q") (symbol->string (var-s x))))))]
                        [(symbol=? 'U which)
                         (not (and (boolean? (regexp-match (regexp "p") (symbol->string (var-s x))))
                                   (boolean? (regexp-match (regexp "q") (symbol->string (var-s x))))))]))
                    vars))
            (remove-duplicates
             (lambda(alist)
               (cond
                 [(empty? alist) '()]
                 [(foldr (lambda(x y) (or x y)) false (map (lambda(x) (eqn=? (car alist) x)) (cdr alist)))
                  (remove-duplicates (cdr alist))]
                 [else (cons (car alist) (remove-duplicates (cdr alist)))]))))
      (cond
        [(symbol=? 'X which) pass1]
        [(symbol=? 'U which)
         (remove-duplicates
          (map
           (lambda(x)
             (if (not (boolean? (regexp-match (regexp "dot") (symbol->string (var-s x)))))
                 (make-var (string->symbol (substring (symbol->string (var-s x)) 0 
                                                      (- (string-length (symbol->string (var-s x))) 3))))
                 x))
           pass1))]))))

;sort-mvars: graph (listof eqns) symbol -> (listof var)
;same as get-mvars, but sorts them into an appropriate order
(define sort-mvars
  (lambda(agraph eqns which)
    (let*
        ((vars (get-mvars agraph eqns which))
         (svars (filter
                 (lambda(x)
                   (let ((y (symbol->string (var-s x))))
                     (not (and (boolean? (regexp-match (regexp "p") y))
                               (boolean? (regexp-match (regexp "q") y)))))) vars))
         (pvars (filter
                 (lambda(x)
                   (let ((y (symbol->string (var-s x))))
                     (and (boolean? (regexp-match (regexp "p") y))
                          (boolean? (regexp-match (regexp "q") y))))) vars))
         (get-num
          (lambda(x)
            (let((y (symbol->string (var-s x))))
              (cond
                [(not (boolean? (regexp-match (regexp "dot") y)))
                 (string->number (substring y 1 (- (string-length y) 3)))]
                [(not (boolean? (regexp-match (regexp "effort") y)))
                 (string->number (substring y 6))]
                [(not (boolean? (regexp-match (regexp "flow") y)))
                 (string->number (substring y 4))]
                [else (string->number (substring y 1))]))))
         (svarsnum (map (lambda(x) (list x (get-num x))) svars))
         (pvarsnum (map (lambda(x) (list x (get-num x))) pvars))
         (sortsvars (sort svarsnum (lambda(x y) (< (cadr x) (cadr y)))))
         (sortpvars (sort pvarsnum (lambda(x y) (< (cadr x) (cadr y))))))
      (append (map car sortsvars) (map car sortpvars)))))

;get-var-matrix: graph (listof eqn) symbol -> (vectorof (vector var))
;creates either matrix X or U for a graph, depending on the symbol
(define get-var-matrix
  (lambda(agraph eqns which)
    (let
        ((alist
          (if (symbol=? 'X which)
              (sort-mvars agraph eqns which)
              (append (sort-mvars agraph eqns which) (list 1)))))
      (build-vector (length alist) (lambda(x) (vector (list-ref alist x)))))))

;get-coefficient : (list var (listof (op 'mul num var))) (var OR number) -> number
;gets the coefficient of a single variable in a list-type equation
(define get-coefficient
  (lambda(leqn avar)
    (cond
      [(number? avar)
       (apply + (map eqn->num (filter num? (cadr leqn))))]
      [else
       (let
           ((get-co
             (lambda(bmul bvar)
               (if (op? bmul) (eqn=? (op-right bmul) bvar) false)))
            (astr (symbol->string (var-s avar))))
         (+ (* (apply + (map (lambda(x) (num-n (extract-coefficient x)))
                             (filter (lambda(x) (get-co x avar)) (cadr leqn))))
               (if
                (and (not (and (boolean? (regexp-match (regexp "p") astr))
                               (boolean? (regexp-match (regexp "q") astr))))
                     (boolean? (regexp-match (regexp "dot") astr)))
                1 -1))
            (if (eqn=? (car leqn) avar) 1 0)))])))

;get-coefficient-matrix: (listof eqns) (vectorof (vector var)) -> (vectorof (vectorof number))
;creates either coefficient matrix 'A or 'B, depending on whether X or U is passed
(define get-coefficient-matrix
  (lambda(eqns vars)
    (let*
        ((listmatrix
          (build-list (vector-length vars)
                      (lambda(x)
                        (build-list (length eqns)
                                    (lambda(y)
                                      (get-coefficient (list-ref eqns y)
                                                       (vector-ref (vector-ref vars x) 0)))))))
         (ltv
          (lambda(alol)
            (if (empty? alol) alol
                (build-vector (length (car alol))
                              (lambda(x)
                                (build-vector (length alol)
                                              (lambda(y)
                                                (list-ref (list-ref alol y) x)))))))))
      (ltv listmatrix)
      )))

;graph->matrix: graph -> (listof (vectorof (vectorof number)))
;converts a bond graph into a matrix equation, AX=BU
(define graph->matrix
  (lambda(agraph)
    (let*
        ((bgraph (graphbondcheck (sort-graph agraph)))
         (aeqns (graph->eqns bgraph))
         (X (get-var-matrix bgraph aeqns 'X))
         (U (get-var-matrix bgraph aeqns 'U))
         (A (get-coefficient-matrix (map simplify-eqn (substitute-all bgraph aeqns)) X))
         (B (get-coefficient-matrix (map simplify-eqn (substitute-all bgraph aeqns)) U)))
      (list A X B U))))
