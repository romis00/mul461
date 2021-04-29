#lang racket
(provide (all-defined-out)) ;; we will import this code and run tests in a different file

;; mul (made up language) part a
;; int and boolean expressions and operations

;; int constant and operations on ints
(struct mul461-int (value) #:transparent)
(struct mul461-+ (e1 e2) #:transparent) ;; given
(struct mul461-- (e1 e2) #:transparent)
(struct mul461-* (e1 e2) #:transparent)

;; boolean constant and operations on booleans
(struct mul461-bool (value) #:transparent)
(struct mul461-and (e1 e2) #:transparent)
(struct mul461-or (e1 e2) #:transparent)
(struct mul461-not (e) #:transparent)

;; comparisons between two int expressions
(struct mul461-< (e1 e2) #:transparent)
(struct mul461-= (e1 e2) #:transparent)

;; first evaluates e1 to a boolean value, if it is true, the whole expression evaluates to e2
;; otherwise the whole expression evaluates to e3
(struct mul461-if (e1 e2 e3) #:transparent)

;; variable and let
(struct mul461-var (name) #:transparent) ;; given
(struct mul461-let (name e body) #:transparent)

;; pair and list
(struct mul461-null () #:transparent)
(struct mul461-cons (e1 e2) #:transparent)
(struct mul461-car (e) #:transparent)
(struct mul461-cdr (e) #:transparent)
(struct mul461-isnull (e) #:transparent)

;; function declaration and function call
(struct mul461-fun (fname formal body) #:transparent)
(struct mul461-call (funexp actual) #:transparent)

;; closure is not a mul461-exp in the source code
;; it is a value that a mul461-fun evaluates to.
(struct mul461-closure (fun env) #:transparent)

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of mul461 expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  	(cond [(mul461-var? e) (envlookup env (mul461-var-name e))]
              [(mul461-int? e) (mul461-int (mul461-int-value e))]
              [(mul461-bool? e) (mul461-bool (mul461-bool-value e))]
              [(mul461-null? e) e]
              [(mul461-+? e) (let (
				   [v1 (eval-under-env (mul461-+-e1 e) env)]
              		           [v2 (eval-under-env (mul461-+-e2 e) env)]
				  )
           			  (
				    if (and (mul461-int? v1) (mul461-int? v2))
               			       (mul461-int (+ (mul461-int-value v1) (mul461-int-value v2)))
               			       (error "mul461-+ applied to non-integer")
				  )
			     )
	      ]
	      [(mul461--? e) (let (
				   [v1 (eval-under-env (mul461---e1 e) env)]
				   [v2 (eval-under-env (mul461---e2 e) env)]
				  )
				  (
				   if (and (mul461-int? v1) (mul461-int? v2))
				      (mul461-int (- (mul461-int-value v1) (mul461-int-value v2)))
				      (error "mul461-- applied to non-integer")
				  )
			     )
	      ]
	      [(mul461-*? e) (let (
				   [v1 (eval-under-env (mul461-*-e1 e) env)]
				   [v2 (eval-under-env (mul461-*-e2 e) env)]
				  )
				  (
				   if (and (mul461-int? v1) (mul461-int? v2))
				      (mul461-int (* (mul461-int-value v1) (mul461-int-value v2)))
				      (error "mul461-* applied to non-integer")
				  )
			     )
	      ]
	      [(mul461-<? e) (let (
				   [v1 (eval-under-env (mul461-<-e1 e) env)]
				   [v2 (eval-under-env (mul461-<-e2 e) env)]
				  )
				  (
				   if (and (mul461-int? v1) (mul461-int? v2))
				      (mul461-bool (if (< (mul461-int-value v1) (mul461-int-value v2)) #t #f))
				      (error "mul461-< applied to non-integer")
				  )
			     )
	      ]
	      [(mul461-=? e) (let (
				   [v1 (eval-under-env (mul461-=-e1 e) env)]
				   [v2 (eval-under-env (mul461-=-e2 e) env)]
				  )
				  (
				   if (and (mul461-int? v1) (mul461-int? v2))
				      (mul461-bool (if (= (mul461-int-value v1) (mul461-int-value v2)) #t #f))
				      (error "mul461-= applied to non-integer")
				  )
			     )
	      ]
	      [(mul461-and? e) (let (
				     [v1 (eval-under-env (mul461-and-e1 e) env)]
				     [v2 (eval-under-env (mul461-and-e2 e) env)]
				    )
				    (
				     if (and (mul461-bool? v1) (mul461-bool? v2))
				        (mul461-bool (if (and (mul461-bool-value v1) (mul461-bool-value v2)) #t #f))
				        (error "mul461-and applied to non-boolean")
				    )
			       )
	      ]
	      [(mul461-or? e) (let (
				    [v1 (eval-under-env (mul461-or-e1 e) env)]
				    [v2 (eval-under-env (mul461-or-e2 e) env)]
				   )
				   (
				    if (and (mul461-bool? v1) (mul461-bool? v2))
				       (mul461-bool (if (or (mul461-bool-value v1) (mul461-bool-value v2)) #t #f))
				       (error "mul461-or applied to non-boolean")
				   )
			      )
	      ]
	      [(mul461-not? e) (let (
				     [v1 (eval-under-env (mul461-not-e e) env)]
				    )
				    (
				     if (mul461-bool? v1)
				        (mul461-bool (if (mul461-bool-value v1) #f #t))
				        (error "mul461-not applied to non-boolean")
				    )
			       )
	      ]
	      [(mul461-if? e) (let (
				    [v1 (eval-under-env (mul461-if-e1 e) env)]
				   )
				   (
				    if (mul461-bool? v1)
				       (if (mul461-bool-value v1) (eval-under-env (mul461-if-e2 e) env) (eval-under-env (mul461-if-e3 e) env))
				       (error "mul461-if applied to non-boolean")
				   )
			      )
	      ]
	      [(mul461-let? e) (let (
                                     [v1 (mul461-let-name e)]
                                     [v2 (eval-under-env (mul461-let-e e) env)]
				    )
                                    (eval-under-env (mul461-let-body e) (cons (cons v1 v2) env))
			       )
	      ]
              [(mul461-cons? e) (let (
                                     [v1 (eval-under-env (mul461-cons-e1 e) env)]
				     [v2 (eval-under-env (mul461-cons-e2 e) env)]
				     )
                                     (mul461-cons v1 v2)
			        )
	      ]
              [(mul461-car? e) (let (
				     [v1 (eval-under-env (mul461-car-e e) env)]
				    )
				    (
				     if (mul461-cons? v1)
				        (mul461-cons-e1 v1)
				        (error "mul461-car applied to non-cons")
				    )
			       )
              ]
              [(mul461-cdr? e) (let (
				     [v1 (eval-under-env (mul461-cdr-e e) env)]
				    )
				    (
				     if (mul461-cons? v1)
				        (mul461-cons-e2 v1)
				        (error "mul461-cdr applied to non-cons")
				    )
			       )
              ]
              [(mul461-isnull? e) (let (
				     [v1 (eval-under-env (mul461-isnull-e e) env)]
				    )
				    (
				     if (mul461-null? v1)
				        (mul461-bool #t)
                                        (mul461-bool #f)
				    )
			       )
              ]
              [(mul461-fun? e) (mul461-closure e env)]
              [(mul461-closure? e)  (let (
				          [v1 (mul461-closure-fun e)]
                                          [v2 (mul461-closure-env e)]
                                         )
				         (
				          (mul461-closure v1 v2)
				         )
			            )
              ]
              [(mul461-call? e) (let (
                                      [v1 (eval-under-env (mul461-call-funexp e) env)]
                                      [v2 (eval-under-env (mul461-call-actual e) env)]
				     )
                                     (
		if (mul461-closure? v1)
                   (eval-under-env
                              (mul461-fun-body (mul461-closure-fun v1))
                   (cons (cons (mul461-fun-fname (mul461-closure-fun v1)) v1)
                   (cons (cons (mul461-fun-formal (mul461-closure-fun v1)) v2) (mul461-closure-env v1))))
                   (error "mul461-call applied to non-closure")
				     )
			        )
	      ]
	      [#t (error (format "bad mul461 expression: ~v" e))]))


;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; CHANGE and implement makelet* and makefactorial
(define (makelet* t e) (eval-under-env e t))
(define (makefactorial n) (if (<= n 1) (mul461-int 1) (mul461-* (mul461-int n) (makefactorial (- n 1)))))

;; A sample mul461 function factorial
(define mul461-factorial
  (mul461-fun "factorial" "n"
              (mul461-if (mul461-< (mul461-var "n") (mul461-int 2))
                         (mul461-int 1)
                         (mul461-*
                          (mul461-var "n")
                          (mul461-call (mul461-var "factorial")
                                       (mul461-- (mul461-var "n") (mul461-int 1)))))))
;; CHANGE and implement makelist
(define (makelist t)
                      (if (null? t) (mul461-null) (mul461-cons (car t) (makelist (cdr t))))
)

;; CHANGE and define mul461 functions filter and map:
(define mul461-filter
  (mul461-fun "filter" "f"
     (mul461-fun "filterf" "t" (mul461-if (mul461-isnull (mul461-var "t")) (mul461-null)
                                          (mul461-if (mul461-call (mul461-var "f") (mul461-car (mul461-var "t")))
                                                     (mul461-cons (mul461-car (mul461-var "t")) (mul461-call (mul461-var "filterf") (mul461-cdr (mul461-var "t"))))
                                                     (mul461-call (mul461-var "filterf") (mul461-cdr (mul461-var "t"))))
                               )
     )
  )
)

(define mul461-map
  (mul461-fun "map" "f"
     (mul461-fun "mapf" "t" (mul461-if (mul461-isnull (mul461-var "t")) (mul461-null)
                                       (mul461-cons (mul461-call (mul461-var "f") (mul461-car (mul461-var "t")))
                                                    (mul461-call (mul461-var "mapf") (mul461-cdr (mul461-var "t"))))))))

