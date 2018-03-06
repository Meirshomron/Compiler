(load "project/qq.scm")


(define drop-last			 			;input:list/improper list ;output:list without the last element.
	(lambda (lst)
		(if (not (pair? lst))
				'()
			(if (null? lst)
				lst
				(append (list (car lst)) (drop-last (cdr lst)))))))

(define check-duplicates				;input:list ;output:if the list has duplicates.			
	(lambda (lst)
		(if (null? lst)
			#f
		(let ((head (car lst))
			  (tail (cdr lst)))
			(if (member head tail) #t 
				(check-duplicates tail))))))


(define let*->let 						;input:let* expression ;output:nested let expression.
	(lambda (args vals body)
		(if (null? args) `(let () ,@body)
 		(if (=(length args) 1)
  			`(let ((,(car args) ,(car vals))),@body)
		(let ((first-arg (car args))
			  (rest-args (cdr args))
			  (first-val (car vals))
			  (rest-vals (cdr vals)))
			`(let ((,first-arg ,first-val)) ,(let*->let rest-args rest-vals body)  ))))))


(define del (lambda(V L) 				;input:string,list expression ;output:list without the string and also flattens every appearence.
  (cond ((null? L) L)
        ((and (list? (car L))  (equal? V (caar L)))
         (del V (append (del V (cdar L)) (cdr L))))
        ((and (not (list? (car L)))  (equal? V (car L)))
         (del V (cdr L)))
        (else (append (list (car L)) (del V (cdr L))))))) 


(define *reserved-words*
	'(and begin cond define do else if lambda
	let let* letrec or quasiquote unquote
	unquote-splicing quote set!))

(define parse
  ;; fill in the definition of the tag parser here
  (lambda (exp)
  	(letrec 

  		((parse-loop (lambda (sexp)
  		(cond 
  		((null? sexp) (list 'const '()))	
  		((quote? sexp)
  			
  			(list 'const (cadr sexp)))
  		((or (const? sexp ) (vector? sexp))
  			(list 'const sexp))
  		((and (not (member sexp *reserved-words* ))(symbol? sexp))
  			(list 'var sexp))
  		((list? sexp)
  			(cond 	((eq? (car sexp) 'if)
  						`(if3 ,(parse-loop (cadr sexp)) ,(parse-loop (caddr sexp)) 
  							,(if (null? (cdddr sexp) ) (list 'const (if #f #f))(parse-loop (cadddr sexp)))))
  					((eq? (car sexp) 'or) 
  						(if (null? (cdr sexp)) (list 'const #f)
  							(if (= ( length (cdr sexp) ) 1) (parse-loop (cadr sexp))
  						`(or ,(map parse-loop (cdr sexp))))))

  					((eq? (car sexp) 'set!) 
  						`(set ,(parse-loop (cadr sexp)) ,(parse-loop (caddr sexp))))

  					((eq? (car sexp) 'define) 
  						(if (pair? (cadr sexp))
  							`(define ,(parse-loop (caadr sexp)) ,(parse-loop `(lambda  ,(cdadr sexp) ,@(cddr sexp ))))
  							`(define ,(parse-loop (cadr sexp)),(if(and(pair? (cddr sexp))(= (length(cddr sexp)) 1))(parse-loop (caddr sexp))(list 'seq (map parse-loop (cddr sexp))) ))))

  					((eq? (car sexp) 'lambda)
  						(if (list? (cadr sexp))
  							(if (not (check-duplicates (drop-last (cadr sexp))))
  								`(lambda-simple ,(cadr sexp) ,(if(= (length(cddr sexp )) 1)(car(map parse-loop (cddr sexp)))(list 'seq (map parse-loop (cddr sexp))))) ; simple
  								"ERROR")
  						(if (pair? (cadr sexp))
  							(if (not (check-duplicates (drop-last (cadr sexp))))
  							`(lambda-opt ,(drop-last (cadr sexp)) ,(cdr (last-pair (cadr sexp))) ,(if(= (length(cddr sexp )) 1)(car(map parse-loop (cddr sexp)))(list 'seq (map parse-loop (cddr sexp))))) ; opt args
  							"ERROR")
  							`(lambda-opt ,'() ,(cadr sexp) ,(if(= (length(cddr sexp )) 1)(car(map parse-loop (cddr sexp)))(list 'seq (map parse-loop (cddr sexp)))))	; ver lambda
  							)
  						))

  					((eq? (car sexp) 'and) 
  						(if (null? (cdr sexp)) (list 'const #t)
  							(if (= ( length (cdr sexp) ) 1) (parse-loop (cadr sexp))
  								(if (= ( length (cdr sexp) ) 2) `(if3 ,(parse-loop (cadr sexp)) ,(parse-loop (caddr sexp)) ,(list 'const #f))
  						`(if3 ,(parse-loop (cadr sexp)) ,(parse-loop (cons 'and (cddr sexp)))  ,(list 'const #f))))))

  					((eq? (car sexp) 'cond) 
  						(if (null? (cdr sexp)) "ERROR"
  							(if (and (= ( length (cdr sexp) ) 1)(eq? (caadr sexp) 'else)) (if(= (length (cdadr sexp)) 1)(parse-loop (cadadr sexp))(list 'seq (map parse-loop (cdadr sexp))))

  							(if (= ( length (cdr sexp) ) 1) `(if3 ,(parse-loop (caadr sexp)) ,(if(= (length (cdadr sexp)) 1)(parse-loop(cadadr sexp))(list 'seq (map parse-loop (cdadr sexp)))) ,(list 'const (if #f #f)))	
  							(if (and (= ( length (cdr sexp) ) 2)(eq? (caaddr sexp) 'else)) `(if3 ,(parse-loop (caadr sexp)) ,(if(= (length (cdadr sexp)) 1)(parse-loop(cadadr sexp))(list 'seq (map parse-loop (cdadr sexp)))) ,(if(= (length (cdaddr sexp)) 1)(parse-loop (car(cdaddr sexp)))(list 'seq (map parse-loop (cdaddr sexp))) ))             
  						`(if3 ,(parse-loop (caadr sexp)) ,(if(= (length (cdadr sexp)) 1)(parse-loop(cadadr sexp))(list 'seq (map parse-loop (cdadr sexp)))) ,(parse-loop (cons 'cond (cddr sexp)))))))))

  					((eq? (car sexp) 'let) 
  						(let ((args (map (lambda(x)(car x)) (cadr sexp)))
  							  (vals (map (lambda(x)(cadr x)) (cadr sexp)))
  							  (body (cddr sexp)))
  							(if  (check-duplicates args) "ERROR"			
  							(parse-loop `((lambda  ,args ,@body) ,@vals)))

						))
  					((eq? (car sexp) 'let*) 
  						(let ((args (map (lambda(x)(car x)) (cadr sexp)))
  							  (vals (map (lambda(x)(cadr x)) (cadr sexp)))
  							  (body (cddr sexp)))

  							  (parse-loop (let*->let args vals body))
  					))
  					((eq? (car sexp) 'letrec) 
  						(let ((args (map (lambda(x)(car x)) (cadr sexp)))
  							  (vals (map (lambda(x)(cadr x)) (cadr sexp)))
  							  (body (cddr sexp)))

  							  (parse-loop `(let ,(map (lambda(arg) `(,arg #f))   args)  
  							  	,@(map (lambda(arg val) `(set! ,arg ,val))   args vals)
  							  	(let () ,@body )))

  					))
  					((eq? (car sexp) 'begin) 
  							(let ((without (del 'begin sexp)))
  								(if(= (length without) 0) (list 'const (void))
  								(if(= (length without) 1)
  									 (parse-loop (car without))
  							  `(seq ,(map parse-loop without)))))
  					)
  					((eq? (car sexp) 'quasiquote) 
  						(parse-loop (expand-qq (cadr sexp)))
  					)


  				(else 
  					`(applic ,(parse-loop (car sexp)) ,(map parse-loop (cdr sexp)) ))
  						 
  			))
  
  ))))
  		(parse-loop exp)

  	)
  	))									


