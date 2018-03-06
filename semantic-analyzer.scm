
(define index-of 
  (lambda (lst e)
    (cond ((equal? e (car lst)) 0)
          (else (+ (index-of (cdr lst) e) 1)))))

(define get
  (lambda (lst index)

    (cond ((equal? index 0)  (car lst))
          (else (get (cdr lst) (- index 1))))))

; (define remove-applic-lambda-nil
;   (lambda(exp)
;      (newline)(display "exp : ")(display exp)(newline)
;     (if(or(null? exp) (not (list? exp))) exp

;   	  (if (and(list? exp)(equal? (car exp) 'applic)(equal? (caadr exp) 'lambda-simple)(equal? (cadadr exp) '())(equal? (caddr exp) '()))
;  			  (remove-applic-lambda-nil (car (cddadr exp)))
;         (if (equal? (car exp) 'applic)
;           (let* ((proc (cadr exp))
;                  (args (caddr exp)))
;                (newline)(display "inside-app proc: ")(display proc)(newline)
;                (newline)(display "inside-app args: ")(display args)(newline)

;                  `(applic ,(remove-applic-lambda-nil proc) ,(map remove-applic-lambda-nil args)))
;   			  (cons (remove-applic-lambda-nil (car exp))(map remove-applic-lambda-nil (cdr exp))))
;   		)
;   	) 
;   )
; )

(define second 
  (lambda (exp) 
    (cadr exp)))

(define third 
  (lambda (exp) 
    (caddr exp)))

(define fourth 
  (lambda (exp) 
    (cadddr exp)))


(define empty? 
  (lambda (exp) 
    (= (length exp) 0)))

; (define applic-remove-applic-lambda-nil
;     (lambda (exp)
;       (newline)(display "applic-exp : ")(display exp)(newline)
;         (if (and (equal? (car (second exp)) 'lambda-simple)
;                 (empty? (second (second exp)))
;                 (empty? (third exp)))
;             ;(empty? (third exp))
;             (remove-applic-lambda-nil (third (second exp)))
;             (map remove-applic-lambda-nil exp))))

; (define remove-applic-lambda-nil
;     ;; fill in the empty-lambda application elimination details here
;     (lambda (exp)

;         (cond   ((not (list? exp)) exp)
;                 ((empty? exp) exp)
;                 ((equal? (car exp) 'lambda-simple)
;                 `(lambda-simple ,(second exp) ,(remove-applic-lambda-nil (third exp))))
;                 ((equal? (car exp) 'lambda-opt)
;                 `(lambda-opt ,(second exp) ,(third exp) ,(remove-applic-lambda-nil (fourth exp))))
;                 ((equal? (car exp) 'applic) (applic-remove-applic-lambda-nil exp))
;                 (else (map remove-applic-lambda-nil exp)))))


(define remove-applic-lambda-nil
  (lambda (exp)
    (cond
      ((null? exp) exp)
      ((not (list? exp)) exp)
      ((list? (car exp)) (cons 
                (remove-applic-lambda-nil (car exp)) 
                (remove-applic-lambda-nil (cdr exp))))
      ((and (equal? 'applic (car exp)) 
          (equal? (car (car (cdr exp))) 'lambda-simple) 
          (null? (cadr (car (cdr exp))))
          (null? (caddr exp))
          ) 
            (remove-applic-lambda-nil (cadr (cdr (car (cdr exp))))))
      (else
        (cons (car exp) (remove-applic-lambda-nil (cdr exp)))
      )
    )
    )
)



; (define applic-remove-applic-lambda-nil
;   (lambda (exp)
;      (newline)(display "applic-exp : ")(display exp)(newline)
;     (if (and (equal? (car (second exp)) 'lambda-simple ) 
;            (empty? (second (second exp)))
;            (empty? (third exp)))
;       (remove-applic-lambda-nil (third (second exp)))
;       (map remove-applic-lambda-nil exp))
;       ))


; (define remove-applic-lambda-nil
;   (lambda(exp)
;      (newline)(display "exp : ")(display exp)(newline)
;     (cond ((not (list? exp)) exp )
;            ((empty? exp) exp)
;            ((equal? (car exp) 'lambda-simple) 
;              `(lambda-simple ,(second exp) ,(remove-applic-lambda-nil (third exp))  ))
;             ((equal? (car exp) 'lambda-opt) 
;              `(lambda-opt ,(second exp) ,(third exp) ,(remove-applic-lambda-nil (fourth exp))  ))
;              ((equal? (car exp) 'applic) (applic-remove-applic-lambda-nil exp))
;              (else (map remove-applic-lambda-nil exp)))))


(define remove
  (lambda (lst x) 
    (cond ((equal? x (car lst)) (cdr lst))
      (else (cons (car lst) (remove (cdr lst) x) )))))

(define is-pvar 
  (lambda (arg expr)

    (cond ((or (null? expr) (not (list? expr)) (and (list? expr)(equal? (car expr) '())) ) #f)

          ((equal? 'var (car expr)) 
            (if (equal? (cadr expr) arg) #t #f))

          ((equal? 'set (car expr)) 
            (is-pvar arg (caddr expr)))

           ((equal? (car expr) 'lambda-simple) #f)
            ((equal? (car expr) 'applic)
              (let* ((proc (cadr expr))
                     (args (caddr expr)))
                    (or (is-pvar arg proc) (fold-left (lambda(acc curr)(or acc (is-pvar arg curr))) #f args))))
            (else (if(equal? (car expr) 'seq)
                  (fold-left (lambda(acc curr)(or acc (is-pvar arg curr))) #f (cadr expr))
                  (or (is-pvar arg (car expr))(fold-left (lambda(acc curr)(or acc (is-pvar arg curr))) #f (cdr expr)))))
      )
    )
  )

(define is-bound 
  (lambda (arg expr pred)

    (cond ((or (null? expr) (and (list? expr)(equal? (car expr) '()))(and (list? expr)(equal? (car expr) 'const)) ) #f)
          ((and (list? expr)(equal? 'var (car expr))) 
            (if (and pred (equal? (cadr expr) arg)) #t #f))
           ((and (list? expr)(equal? (car expr) 'set))
            (or (is-bound arg (cadr expr) pred)(is-bound arg (caddr expr) pred)))
           ((equal? (car expr) 'lambda-simple)
              (let* ((new-params (cadr expr))
                     (new-body (cddr expr)))
                (if(member arg new-params)
                    #f
                 (fold-left (lambda(acc curr)(or acc (is-bound arg curr #t))) #f new-body)
                  )
             ))
            ((equal? (car expr) 'lambda-opt)
              (let* ((new-params (if(null? (cadr expr))(list (caddr expr))(append (cadr expr) (list (caddr expr))) ))

                     (new-body (cdddr expr)))
                (if(member arg new-params)
                    #f
                 (fold-left (lambda(acc curr)(or acc (is-bound arg curr #t))) #f new-body)
                  )
             ))
           ((equal? (car expr) 'applic)
              (let* ((proc (cadr expr))
                     (args (caddr expr)))
                    (or (is-bound arg proc pred) (fold-left (lambda(acc curr)(or acc (is-bound arg curr pred))) #f args))))
             (else (if(equal? (car expr) 'seq)
                  (fold-left (lambda(acc curr)(or acc (is-bound arg curr pred ))) #f (cadr expr))
                  (fold-left (lambda(acc curr)(or acc (is-bound arg curr pred))) #f (cdr expr))))
      )
    )
  )

(define is-bvar 
  (lambda (arg expr pred)

    (cond ((or (null? expr) (and (list? expr)(equal? (car expr) '()))(and (list? expr)(equal? (car expr) 'const)) ) #f)
          ((and (list? expr)(equal? 'var (car expr))) 
            (if (and pred (equal? (cadr expr) arg)) #t #f))
           ((and (list? expr)(equal? (car expr) 'set))
            (is-bvar arg (caddr expr) pred))
           ((equal? (car expr) 'lambda-simple)
              (let* ((new-params (cadr expr))
                     (new-body (cddr expr)))
                (if(member arg new-params)
                    #f
                 (fold-left (lambda(acc curr)(or acc (is-bvar arg curr #t))) #f new-body)
                  )
             ))
           ((equal? (car expr) 'lambda-opt)
              (let* ((new-params (if(null? (cadr expr))(list (caddr expr))(append (cadr expr) (list (caddr expr))) ))

                     (new-body (cdddr expr)))
                (if(member arg new-params)
                    #f
                 (fold-left (lambda(acc curr)(or acc (is-bvar arg curr #t))) #f new-body)
                  )
             ))

           ((equal? (car expr) 'applic)
              (let* ((proc (cadr expr))
                     (args (caddr expr)))
                    (or (is-bvar arg proc pred) (fold-left (lambda(acc curr)(or acc (is-bvar arg curr pred))) #f args))))
             (else (if(equal? (car expr) 'seq)
                  (fold-left (lambda(acc curr)(or acc (is-bvar arg curr pred ))) #f (cadr expr))
                  (fold-left (lambda(acc curr)(or acc (is-bvar arg curr pred))) #f (cdr expr))))
      )
    )
  )

(define is-set 
  (lambda (arg expr)
    (cond ((or (null? expr) (not (list? expr)) (and (list? expr)(equal? (car expr) '())) ) #f)

          ((and (list? expr)(equal? (car expr) 'set) (equal? 'var (caadr expr))) 
            (if (and (equal? (cadadr expr) arg)) #t (is-set arg (caddr expr))))

           ((equal? (car expr) 'lambda-simple)
              (let* ((new-params (cadr expr))
                     (new-body (cddr expr)))
                (if(member arg new-params)
                    #f
                 (fold-left (lambda(acc curr)(or acc (is-set arg curr))) #f new-body)
                )
             ))
            ((equal? (car expr) 'lambda-opt)
              (let* ((new-params (if(null? (cadr expr))(list (caddr expr))(append (cadr expr) (list (caddr expr))) ))

                     (new-body (cdddr expr)))
                (if(member arg new-params)
                    #f
                 (fold-left (lambda(acc curr)(or acc (is-set arg curr ))) #f new-body)
                  )
             ))

            ((equal? (car expr) 'applic)
              (let* ((proc (cadr expr))
                     (args (caddr expr)))
                    (or (is-set arg proc) (fold-left (lambda(acc curr)(or acc (is-set arg curr))) #f args))))
            (else (if(equal? (car expr) 'seq)
                  (fold-left (lambda(acc curr)(or acc (is-set arg curr))) #f (cadr expr))
                  (fold-left (lambda(acc curr)(or acc (is-set arg curr))) #f (cdr expr))))
      )
    )
  )

(define box-get 
  (lambda (params body)

    (cond ((or (null? body) (not (list? body)) (and (list? body)(equal? (car body) '())) ) body)

          ((and (list? body)(equal? 'var (car body))) 
            (if (member (cadr body) params) `(box-get (var ,(cadr body))) body))

          ((and (list? body)(equal? (car body) 'set) (equal? 'var (caadr body))) 
            `(set ,(cadr body)  ,(box-get params (caddr body))))

           ((equal? (car body) 'lambda-simple)
              (let* ((new-params (cadr body))
                     (new-body (cddr body))
                     (filtered-params (filter (lambda(p) (not (member p new-params))) params)))
                      (cond ((equal? (caar new-body) 'seq)
                        `(lambda-simple ,new-params   (seq ,@(map (lambda(b) (box-get filtered-params b)) (cadar new-body)))))
                       (else `(lambda-simple ,new-params ,@(map (lambda(b) (box-get filtered-params b)) new-body)) ) )       
             ))
             ((equal? (car body) 'applic)
              (let* ((proc (cadr body))
                     (args (caddr body)))
                    `(applic ,(box-get params proc) ,(map (lambda(arg )(box-get params arg)) args))))
            (else (if(equal? (car body) 'seq)
                   `(,(car body),(map (lambda(b)(box-get params b)) (cadr body))) 
                  `(,(box-get params (car body)),@(map (lambda(b)(box-get params b)) (cdr body))) ))
      )

    )
  )

(define make-set 
  (lambda (params body)
    (cond ((or (null? body) (not (list? body)) (and (list? body)(equal? (car body) '())) ) body)

          ((and (list? body)(equal? (car body) 'set)(equal? 'var (caadr body))) 
            (if (member (cadadr body) params) `(box-set ,(cadr body) ,(make-set params (caddr body))) `(set ,(cadr body) ,(make-set params (caddr body))) ))

           ((equal? (car body) 'lambda-simple)

              (let* ((new-params (cadr body))
                     (new-body (cddr body))
                     (filtered-params (filter (lambda(p) (not (member p new-params))) params)))
                      (cond ((equal? (caar new-body) 'seq)
                        `(lambda-simple ,new-params  (seq ,(map (lambda(b) (make-set filtered-params b)) (cdar new-body)))))
                       (else `(lambda-simple ,new-params ,@(map (lambda(b) (make-set filtered-params b)) new-body)) ) )    
             ))
           ((equal? (car body) 'applic)
              (let* ((proc (cadr body))
                     (args (caddr body)))
                    `(applic ,(make-set params proc) ,(map (lambda(arg )(make-set params arg)) args))))

            (else (if(equal? (car body) 'seq)
                   `(,(car body),(map (lambda(b)(make-set params b)) (cadr body))) 
                  `(,(car body),@(map (lambda(b)(make-set params b)) (cdr body))) ))
      )
    )
  )

(define box-it
  (lambda (filtered-params body)  
    (let* ((body-after-get (box-get filtered-params body))
           (body-after-set (make-set filtered-params body-after-get)))
          body-after-set 
         )))

  (define get-statemet
    (lambda(bool-list params  body)
        (if(member #t bool-list)
          (let ((filtered-list (filter (lambda (x) (get bool-list (index-of params x))) params)))
            (map (lambda(p) `(set (var ,p) (box (var ,p))) ) filtered-list)
          )
          '())
      ))

(define box-set
  (lambda(exp)
    (if(list? exp)
      (cond ((equal? (car exp) 'const) exp)
            ((equal? (car exp) 'lambda-simple)
              (let* ((params (cadr exp))
                     (body (cddr exp))
                     (bool-list (map (lambda(param) (and (fold-left (lambda(acc curr)(or acc (is-set param curr))) #f body)(or(fold-left (lambda(acc curr)(or acc (is-pvar param curr))) #f body)(fold-left (lambda(acc curr)(or acc (is-bvar param curr #f))) #f body))(fold-left (lambda(acc curr)(or acc (is-bound param curr #f))) #f body) ))  params))
                     (filtered-params (filter (lambda (x) (get bool-list (index-of params x))) params))
                     (new-body (map (lambda(b) (box-it filtered-params b)) body))
                     (statement (get-statemet bool-list params new-body)))

                      (if(not(null? statement))
                        (if(equal? (caar body) 'seq)
                          `(lambda-simple ,params ,(cons 'seq (list (append statement  (box-set (cadar new-body))))))
                          
                          `(lambda-simple ,params ,(cons  'seq (list (append statement (map box-set new-body))))))
                        (if(equal? (caar body) 'seq)
                        `(lambda-simple ,params ,(cons  'seq (list (map box-set (cadar body)))))
                        `(lambda-simple ,params ,@(map box-set new-body))))))
           ((equal? (car exp) 'lambda-opt)
              (let* ((prev-params (list (cadr exp)(caddr exp)))
                     (params (if(null? (cadr exp))(list (caddr exp))(append (cadr exp) (list (caddr exp))) ))
                     (body (cdddr exp))
                     (bool-list (map (lambda(param) (and (fold-left (lambda(acc curr)(or acc (is-set param curr))) #f body)(or(fold-left (lambda(acc curr)(or acc (is-pvar param curr))) #f body)(fold-left (lambda(acc curr)(or acc (is-bvar param curr #f))) #f body)) (fold-left (lambda(acc curr)(or acc (is-bound param curr #f))) #f body) ))  params))
                     (filtered-params (filter (lambda (x) (get bool-list (index-of params x))) params))

                     (new-body (map (lambda(b) (box-it filtered-params b)) body))
                     (statement (get-statemet bool-list params new-body)))
                      (if(not(null? statement))
                        (if(equal? (caar body) 'seq)
                          `(lambda-opt ,@prev-params ,(cons 'seq (list (append statement  (box-set (cadar new-body))))))
                          
                          `(lambda-opt ,@prev-params ,(cons  'seq (list (append statement (box-set new-body))))))
                        (if(equal? (caar body) 'seq)
                        `(lambda-opt ,@prev-params ,(cons  'seq (list (map box-set (cadar body)))))
                        `(lambda-opt ,@prev-params ,@(map box-set new-body)))) ))
            ((equal? (car exp) 'applic)
              (let* ((proc (cadr exp))
                     (args (caddr exp)))

                    `(applic ,(box-set proc) ,(map box-set args))))
            ((equal? (car exp) 'if3) 
              (let  ((test (cadr exp))
                     (then (caddr exp))
                     (else (cadddr exp)))
                    `(if3 ,(box-set test) ,(box-set then) ,(box-set else) )))
            ((equal? (car exp) 'define)
              (let* ((variable (cadr exp))
                     (arg (caddr exp)))
                    `(define ,variable ,(box-set arg))))

            (else (cons (car exp )(map box-set (cdr exp))))
     )
   exp  
   )
  )
)

(define lex
	(lambda (params indexs levels body)
		(letrec ((loop 
			(lambda (b)
				(cond ((null? b) '())
          ((and (list? b)(equal? 'const (car b))) b)
          ((and (list? b)(equal? 'var (car b)))
					(let ((var (cadr b)))
					(if (and (member var params) )
            ; 
            (let* ((position (index-of  params var))
                  (index (get indexs position ))
                  (level (get levels position )))
              (if (= level -1 ) `(pvar ,var ,index ) `(bvar ,var ,level ,index)))
            `(fvar ,var))
            )
					)
          ;lambda-simple
					((and(list? b)(equal? (car b) 'lambda-simple))
            (let* ((cur-params params)
                  (new-params (cadr b))
                  (pred-old-params (map (lambda (var) (member var new-params) )params ) )
                  (pred-new-params (map (lambda (var) (member var params) )new-params ) )
                  (cur-body (cddr b))
                   (cur-levels (map (lambda (var pred) (if pred -1 (+ 1 (get levels (index-of params var)) ))) params pred-old-params) )
                   (cur-indexs (map (lambda (var pred) (if pred (index-of new-params var) (get indexs (index-of params var)) )) params pred-old-params) )
                  )

                  (let ((modify 
                              (lambda (var pred) 
                                  (cond ((not pred) 
                                        (set! cur-params (cons var cur-params))
                                        (set! cur-levels (cons -1 cur-levels))
                                        (set! cur-indexs (cons (index-of new-params var) cur-indexs))
                                         )))))
                  (map modify new-params pred-new-params))

            `(lambda-simple ,new-params ,@(lex cur-params cur-indexs cur-levels cur-body ) ))
						
					)
          ;lambda-opt
          ((and(list? b)(equal? (car b) 'lambda-opt))
            (let* ((cur-params params)
                  (prev-params (list (cadr b)(caddr b)))
                  (new-params (if(null? (cadr b))(list (caddr b))(append (cadr b) (list (caddr b)) ) ))
                  (pred-old-params (map (lambda (var) (member var new-params) )params ) )
                  (pred-new-params (map (lambda (var) (member var params) ) new-params ) )
                  (cur-body (cdddr b))
                   (cur-levels (map (lambda (var pred) (if pred -1 (+ 1 (get levels (index-of params var)) ))) params pred-old-params) )
                   (cur-indexs (map (lambda (var pred) (if pred (index-of new-params var) (get indexs (index-of params var)) )) params pred-old-params) )
                  )

                  (let ((modify 
                              (lambda (var pred) 
                                  (cond ((not pred) 
                                        (set! cur-params (cons var cur-params))
                                        (set! cur-levels (cons -1 cur-levels))
                                        (set! cur-indexs (cons (index-of new-params var) cur-indexs))
                                         )))))
                  (map modify new-params pred-new-params))

            `(lambda-opt ,@prev-params ,@(lex cur-params cur-indexs cur-levels cur-body ) ))
            
          )
        ((equal? (car b) 'applic)
        (let* ((proc (cadr b))
          (args (caddr b)))
         `(applic ,(loop proc) ,(map loop args))))
        ((equal? (car b) 'or)
        (let* ((args (cadr b)))
         `(or ,(map loop args))))
        ((equal? (car b) 'seq)
        (let* ((expr1 (cadr b))
                (expr2 (cddr b)))

          (if(null? expr2) `(seq ,(map loop expr1)) `(seq ,(map loop expr1) ,@(map loop expr2)))))
          (else (cons (car b) (map loop (cdr b))))))))

			(map loop body))
	)
)

(define pe->lex-pe
  ;; fill in the lexical addressing details here
 (lambda (exp)
; (newline)(display "P: ")(display exp)(newline)
    (if (and (list? exp)(equal? 'var (car exp))) `(fvar ,(cadr exp))
  	   (if(list? exp)
  		    (cond 
            ((equal? (car exp) 'const) exp)
            ((equal? (car exp) 'lambda-simple)
  			      (let* ((params (cadr exp))
  				           (body (cddr exp))
                     (levels (map (lambda (x) -1) params))
                     (indexs (map (lambda (x) (index-of params x)) params)))
  		            	`(lambda-simple ,params ,@(lex params indexs levels body))))
            ((equal? (car exp) 'lambda-opt)
              (let* ((prev-params (list (cadr exp)(caddr exp)))
                     (params (if(null? (cadr exp))(list (caddr exp))(append (cadr exp) (list (caddr exp)) ) ))
                     (body (cdddr exp))
                     (levels (map (lambda (x) -1) params))
                     (indexs (map (lambda (x) (index-of params x)) params)))
                   `(lambda-opt ,@prev-params ,@(lex params indexs levels body))))
            ((equal? (car exp) 'applic)
              (let* ((proc (cadr exp))
                     (args (caddr exp)))
                   `(applic ,(pe->lex-pe proc) ,(map pe->lex-pe args))))
            ((equal? (car exp) 'or)
                    (let* ((args (cadr exp)))
                   
                    `(or ,(map pe->lex-pe args))))

            ((equal? (car exp) 'seq)
              (let* ((expr1 (cadr exp))
                     (expr2 (cddr exp)))

                    (if(null? expr2) `(seq ,(map pe->lex-pe expr1)) `(seq ,(map pe->lex-pe expr1) ,@(map pe->lex-pe expr2)))))

  		      (else (cons (car exp )(map pe->lex-pe (cdr exp))))
    	    )
          exp  ))
))

(define annotate-tc
  (lambda (exp )
    (my-annotate-tc exp #f)
  )
  )

(define my-annotate-tc
  (lambda (exp tp)

    (cond 
      ((or(null? exp)(not(list? exp))) exp)
      ((equal? (car exp) 'if3) 
          (let ((test (cadr exp))
                (then (caddr exp))
                (else (cadddr exp)))
            `(if3 ,(my-annotate-tc test #f) ,(my-annotate-tc then tp) ,(my-annotate-tc else tp) )
       ))
       ((equal? (car exp) 'lambda-simple)
          (let* ((body (cddr exp))
                (params (cadr exp)))

                (cond ((equal? (caar body) 'seq)
                    (let* ((bool-list(append (map (lambda (x) #f) (cdadar body))  (list #t)))
                         (expr1 (cadar body))
                         (expr2 (cddar body)))

                      (if(null? expr2)  `(lambda-simple ,params (seq ,(map (lambda (e pred) (if pred (my-annotate-tc e #t) (my-annotate-tc e #f))) expr1 bool-list)))
                       `(lambda-simple ,params (seq ,(map (lambda (e pred) (if pred (my-annotate-tc e #t) (my-annotate-tc e #f))) expr1 (list #f)) 
                                                    ,@(map (lambda (e pred) (if pred (my-annotate-tc e #t) (my-annotate-tc e #f))) expr2  bool-list)))
                        )
                  ))
                  (else `(lambda-simple ,params ,(my-annotate-tc (car body) #t))))
            

       ))
        ((equal? (car exp) 'lambda-opt)
          (let* ((body (cdddr exp))
                (params (cadr exp))
                (rest (caddr exp))
                (bool-list(append (map (lambda (x) #f) (cdr body)) (list #t))))

            `(lambda-opt ,params ,rest ,@(map (lambda (e pred)(if pred (my-annotate-tc e #t) (my-annotate-tc e #f))) body bool-list) )

       ))
       ((equal? (car exp) 'applic)
          (let* ((proc (cadr exp))
                (args (caddr exp)))

            `(,(if tp 'tc-applic 'applic) ,(my-annotate-tc proc #f) ,(map (lambda (arg)(my-annotate-tc arg #f)) args ))
       ))
       ((equal? (car exp) 'define)
          (let* ((variable (cadr exp))
                (arg (caddr exp)))

            `(define ,variable ,(my-annotate-tc arg #f))
       ))

       ((equal? (car exp) 'or)
          (let* ((expr (cadr exp))
                 (bool-list(append (map (lambda (x) #f) (cdr expr)) (list tp))))

            `(or  ,(map (lambda (e pred)(if pred (my-annotate-tc e #t) (my-annotate-tc e #f))) expr bool-list ))
       ))
   
       ((or (equal? (car exp) 'box-set) (equal? (car exp) 'set))
          (let* ((var (cadr exp))
                  (val (caddr exp)))

              `(,(if (equal? (car exp) 'box-set)  'box-set 'set) ,var ,(my-annotate-tc val #f))
       ))

        ((equal? (car exp) 'seq)
              (let* ((expr1 (cadr exp))
                     (expr2 (cddr exp))
                     (bool-list-e1 (map (lambda (x) #f) expr1))
                     (bool-list-e2 (map (lambda (x) #f) expr2))
                     (bool-list (cond ((null? bool-list-e2) 
                                      (append (cdr bool-list-e1) (list tp)))
                                      (else (set! bool-list-e2 (append (cdr bool-list-e1 ) (list tp))    ))))
                     )

                    (if(null? expr2) `(seq ,(map (lambda (e pred) (my-annotate-tc e pred)) expr1 bool-list)) `(seq ,(map (lambda (e pred) (my-annotate-tc e pred)) expr1 bool-list-e1) ,@(map (lambda (e pred) (my-annotate-tc e pred)) expr2 bool-list-e2)))))
       (else exp)
       ) 
      )
  )