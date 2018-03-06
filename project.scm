(load "project/tag-parser.scm")
(load "project/sexpr-parser.scm")
(load "project/semantic-analyzer.scm")
(load "project/inon.scm")

(define pipeline
	(lambda (s)
	((star <sexpr>) s
		(lambda (m r)
			(map (lambda (e)
			  (annotate-tc
				(pe->lex-pe
				(box-set
				(remove-applic-lambda-nil
				(parse e))
      )
      )
      )
    )
			m))
	(lambda (f) 'fail))))

(define file->list
	(lambda (in-file)
		(let ((in-port (open-input-file in-file)))
			(letrec ((run
				(lambda ()
				(let ((ch (read-char in-port)))
				(if (eof-object? ch)
				(begin
				(close-input-port in-port)
				'())
					(cons ch (run)))))))
				(run)))))

(define contains
  (lambda (lst x)
    (fold-left (lambda (acc x) (or acc x)) #f (map (lambda (p) (equal? x (cadr p) )) lst) )
    ))


(define list->set
	(lambda(s)
		(fold-right
			(lambda(a s)
				(if(ormap (lambda(si)(equal? a si)) s)
					s
					(cons a s)))
			'()
			s)))

(define f-you
    (lambda (p)
      (cond  ((symbol? p) 
                (separate-symbol p))
              ((rational? p)
                (separate-fraction p))
              (else (set! const-list (cons p const-list))))  ))

(define separate-symbol
    (lambda (p)
      (set! const-list (cons (symbol->string p) (cons p const-list)))))

(define separate-fraction
    (lambda (p)
    (if (integer? (/ (numerator p) (denominator p)))
      (set! const-list (cons (/ (numerator p) (denominator p))  const-list)) 
      (set! const-list (cons (numerator p) (cons (denominator p) (cons p const-list)) )))  ))

(define separate-list 
	(lambda (p) 
		(cond  
			((vector? p)(separate-vector (vector->list p)))
			((pair? p)
      (set! const-list(cons (f-you p) const-list) ) 
			 (cond 
          ((list? (car p))
         ; (newline)(display "is-list: ")(display p)(newline)
         (if (null? (car p)) (cons  (f-you p ) const-list)
           (begin (set! const-list(cons (f-you(caar p)) (cons (f-you(car p)) const-list) ) )
                           (set! const-list(cons (f-you (caar p)) (cons  (f-you p ) const-list) ) )
                           (separate-list (cdar p))))
                             (separate-list (cdr p))
                             (if (and (not (null? (car p)))(list? (caar p))) (separate-list (caar p)))) 
				  ((pair? (car p))   (set! const-list(cons (f-you(caar p)) (cons (f-you (cadr p)) (cons (f-you (car p)) const-list) ) ) ))
          ((vector? (car p)) (set! const-list(cons (f-you p) const-list) ) (separate-vector (vector->list (car p)) (car p))       )
				(else (set! const-list(cons (f-you (car p)) (cons (f-you p) const-list) ))))
			(separate-list (cdr p)))
		(else 
      (f-you p)
      )

)))

(define separate-vector 
	(lambda (p v) 
	     (set! const-list (cons v const-list))
		 (map (lambda(p1) (if (vector? p1) (separate-vector (vector->list p1) p1) (separate-list p1))) p)
))


(define print-rax-not-void
	 "push qword [rax]\ncall write_sob_if_not_void\nadd rsp, 1*8\n")


(define fvar-list '( apply = + / * - < >
  boolean? car cdr char->integer char? cons  eq? integer?
  integer->char list make-string make-vector map string? not null? number?
  numerator pair? procedure? rational? remainder set-car! set-cdr!
  string-ref string-set! string->symbol string-length symbol? meirlength
  symbol->string vector vector? vector-length denominator vector-ref vector-set! zero? gcd append impl-append my-append list map))
		

(define sym-list '())
(define const-list (cons #t (cons #f (cons '() (cons (void) '())))))
(define sym-names-list '())

(define make-const-table
	(lambda (exp)
		(cond 
      ((or(null? exp)(not(list? exp))   (and (equal? (car exp) 'const)(equal? (cadr exp) (void)))  ) '())
      ((equal? (car exp) 'const)  
      		(let ((const-val (cadr exp)))
      			(cond ((pair? const-val) (separate-list const-val))

              ((symbol? const-val)  (set! const-list (cons (symbol->string const-val) (cons const-val const-list))))


      				  ((vector? const-val)(separate-vector (vector->list const-val) const-val))
                ((rational? const-val)
                  (if (integer? (/ (numerator const-val) (denominator const-val))) (set! const-list (cons (/ (numerator const-val) (denominator const-val))  const-list)) 

                 (set! const-list (cons (numerator const-val) (cons (denominator const-val) (cons const-val const-list)) ))))
      					(else (set! const-list (cons const-val const-list))))
      		 ))
      ((equal? (car exp) 'fvar) 
      	(let ((fvar-val (cadr exp)))
      			(set! fvar-list (cons fvar-val fvar-list))
      		 )
      ) 

      ((equal? (car exp) 'if3) 
          (let ((test (cadr exp))
                (then (caddr exp))
                (else (cadddr exp)))
            (list (make-const-table test) (make-const-table then) (make-const-table else) )
       ))
       ((equal? (car exp) 'lambda-simple)
          (let* ((body (cddr exp))
                (params (cadr exp)))
                (cond ((equal? (caar body) 'seq)
                    (let* ((expr1 (cadar body))
                         (expr2 (cddar body))
                         )
                      (if(null? expr2)  
                      	(map (lambda (e) (make-const-table e)) expr1)
                        (list (map (lambda (e) (make-const-table e)) expr1)
                                                    (map (lambda (e) (make-const-table e)) expr1))
                        )
                  ))
                  (else `(,(make-const-table (car body)))))
            

       ))
        ((equal? (car exp) 'lambda-opt)
          (let* ((body (cdddr exp))
                (params (cadr exp))
                (rest (caddr exp)))
            `(,@(map (lambda (e) (make-const-table e)) body ))

       ))
       ((or (equal? (car exp) 'applic) (equal? (car exp) 'tc-applic))
          (let* ((proc (cadr exp))
                (args (caddr exp)))
            (list (make-const-table proc) (map (lambda (arg)(make-const-table arg)) args ))
       ))
       ((equal? (car exp) 'define)
          (let* ((variable (cadr exp))
                (arg (caddr exp)))

            `(,(make-const-table variable),(make-const-table arg))
       ))

       ((equal? (car exp) 'or)
          (let* ((expr (cadr exp)))
              

            `(,@(map (lambda (e)(make-const-table e)) expr))
       ))
   
       ((or (equal? (car exp) 'box-set) (equal? (car exp) 'set))
          (let* ((var (cadr exp))
                  (val (caddr exp)))
                (set! fvar-list (cons (cadr var) fvar-list))
              `(,(make-const-table val))
       ))
       ((equal? (car exp) 'box-get)
       		(let* ((var (cadr exp)))

             	 `(,(make-const-table var))
       ))

        ((equal? (car exp) 'seq)
              (let* ((expr1 (cadr exp))
                     (expr2 (cddr exp))
                     )

                    (if(null? expr2) `(,@(map (lambda (e) (make-const-table e)) expr1)) (list (map (lambda (e) (make-const-table e pred)) expr1) (map (lambda (e) (make-const-table e)) expr2)))))
       
       (else '())
       ) 
      )
  )





(define str-fraction
  (lambda (label val)
    (let* ((val-numerator (numerator val) ) 
          (val-denomerator (denominator val))
          (val-gcd (gcd val-numerator val-denomerator))
          (val-numerator-gcd (/ val-numerator val-gcd)  )
          (val-denomerator-gcd (/ val-denomerator val-gcd)  )
          )
        (string-append label ": dq MAKE_LITERAL_FRACTION(" (find-label const-list val-numerator-gcd ) ","  (find-label const-list val-denomerator-gcd) ")" )
)))


(define lookup-var 
  (lambda(name clist)
    (if (null? clist) 'error)
      (if (equal? (car (car clist)) name) (car (cdr (car ctable)))
    (lookup-var name (cdr clist)))))  


                                       
(define label-gen 
	(lambda (lst index label)
	(if (null? lst) '() 
		(let ((head (car lst))
			  (tail (cdr lst)))
			(cons `(,(string-append label (number->string index)) ,head) (label-gen tail (+ 1 index) label))
		))))

(define find-label
	(lambda (lst v) 

		(let ((label (caar lst))
			  (val (cadar lst)))
			(if (or(eq? val v) (equal? val v)) label
				(find-label (cdr lst) v))
			)
))

(define clean-string-from-special
  (lambda (str acc last_special)
    (if (= (string-length str) 0) 
      (if last_special
          acc
          (string-append acc "\""))
      (cond ((eq? (string-ref str 0) #\nul)
                  (clean-string-from-special (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_NUL") (string-append acc "\""  ", CHAR_NUL")) #t))
            ((eq? (string-ref str 0) #\tab)
                  (clean-string-from-special (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_TAB") (string-append acc "\""  ", CHAR_TAB")) #t))
            ((eq? (string-ref str 0) #\newline)
                  (clean-string-from-special (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_NEWLINE") (string-append acc "\""  ", CHAR_NEWLINE")) #t))
            ((eq? (string-ref str 0) #\page)
                  (clean-string-from-special (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_PAGE") (string-append acc "\""  ", CHAR_PAGE")) #t))
            ((eq? (string-ref str 0) #\return)
                  (clean-string-from-special (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_RETURN") (string-append acc "\""  ", CHAR_RETURN")) #t))
            ((eq? (string-ref str 0) #\space)
                  (clean-string-from-special (substring str 1 (string-length str)) (if last_special (string-append acc ", CHAR_SPACE") (string-append acc "\""  ", CHAR_SPACE")) #t))

          (else (clean-string-from-special (substring str 1 (string-length str)) (if last_special (string-append acc ", \""(substring str 0 1))(string-append acc (substring str 0 1))) #f))))


    ))
(define clean-char-from-speciel
  (lambda(chr)
    (cond ((eq? chr #\nul) "CHAR_NUL")
          ((eq? chr #\tab) "CHAR_TAB")
          ((eq? chr #\newline) "CHAR_NEWLINE")
          ((eq? chr #\page) "CHAR_PAGE")
          ((eq? chr #\return) "CHAR_RETURN")
          ((eq? chr #\space) "CHAR_SPACE")
          (else (string-append "'" (number->string (char->integer val)) "'")))


    ))


(define build-sym-table-assembly
  (lambda(sym-lst)

     (letrec ((loop (lambda (lst index)
                         (cond  ((null? lst) "")
                                ((if (= index 0 ) (string-append "sym" (number->string index) ": dq MAKE_LITERAL_PAIR("(car lst) "," (find-label const-list '())")\n" (loop (cdr lst) (+ 1 index)))
                                          (string-append "sym" (number->string index) ": dq MAKE_LITERAL_PAIR("(car lst) ", sym" (number->string (- index 1) ) ")\n" (loop (cdr lst) (+ 1 index)) ) )
                                )))))      
       (if (null? sym-lst) (string-append "sym_table: dq "(find-label const-list '())" \n") (string-append (loop (reverse sym-lst) 0) "\nsym_table: dq sym"(number->string (- (length sym-lst) 1))  "\n"  ))
       ) 
))

(define build-const-table-assembly 
	(lambda (lst)
    (string-append "\nsection .data\nstart_of_data:\n" (fold-left (lambda (acc x) (string-append acc "\n" x)) "" 
			(map (lambda (x)

				(let ((label (car x))
					  (val (cadr x))) 
				(cond ((equal? (void) val) (string-append label ": dq SOB_VOID "  ))
					  ((number? val) (if (integer? val) 
									  (string-append label ": dq MAKE_LITERAL(T_INTEGER, " (number->string val) ")" )
									  (str-fraction label val)  ))
					  ((boolean? val) (string-append label ": dq " (if val "SOB_TRUE" "SOB_FALSE") " " ))
					  ((char? val) (string-append label ": dq MAKE_LITERAL(T_CHAR, " (number->string (char->integer val)) ")" ))
					  ((null? val) (string-append label ": dq SOB_NIL "  ))
					  ((pair? val) (string-append label ": dq MAKE_LITERAL_PAIR(" (find-label const-list (car val)) ","  (find-label const-list (cdr val)) ")" ))
					  ((string? val) (string-append label ": MAKE_LITERAL_STRING " (clean-string-from-special val "\"" #f)  ))

             ((symbol? val)(begin  (set! sym-list (cons (find-label const-list val) sym-list))(string-append label ": dq MAKE_LITERAL_SYMBOL("(find-label const-list (symbol->string val))" ) "  )))

					  ((vector? val) (if ( = (vector-length val) 0)(string-append label ": dq MAKE_LITERAL(T_VECTOR,0)\n") (string-append label ": MAKE_LITERAL_VECTOR " 
              (let ((my_list(fold-right (lambda (acc x) (string-append acc "," x)) ""  (map (lambda(v) (find-label const-list v))  (vector->list val)  ))))
					  			(if(equal? "" my_list) my_list (substring  my_list 0 (- (string-length my_list) 1)))
					  		))))
					  (else "\n")
					
				))
			 ) lst) ) "\n"))
)

(define build-fvar-table-assembly 
	(lambda (lst)
		(string-append  (fold-left (lambda (acc x) (string-append acc "\n" x)) "" 
			(map (lambda (x)
				(let ((label (car x))
					  (val (cadr x)))
					  (string-append label": dq SOB_UNDEFINED ") 
				)
			 ) lst))

        "\n" ))
)

(define gen-code-set-pvar
    (lambda (var val major clist flist)
      (let* ((gen-val (code-gen val major clist flist))
             (mi  (caddr var)))
     (string-append 
      gen-val "\n"
      "mov qword [rbp+(4+"(number->string mi)")*8] , rax\n"
      "mov rax, " (find-label clist (void)) "\n"))))

(define gen-code-set-bvar
    (lambda (var val major clist flist)
      (let* ((gen-val (code-gen val major clist flist))
             (ma  (caddr var))
             (mi  (cadddr var)))
     (string-append 
        gen-val "\n"

      "mov rbx, qword [rbp + 2*8]\n" ;env
      "mov rbx, qword [rbx + " (number->string ma) "*8]\n" ;env[ma]
      "mov qword [rbx+"(number->string mi)"*8] , rax\n"
      "mov rax, " (find-label clist (void)) "\n"))))
  
  (define gen-code-set-fvar
    (lambda (var val major clist flist)
      (let* ((gen-val (code-gen val major clist flist)))

     (string-append 
      gen-val "\n"
      "mov ["(find-label flist (cadr var)) "], rax\n"
      "mov rax, " (find-label clist (void)) "\n"))))


(define gen-code-set
    (lambda (exp major clist flist)
      (let* ((var (cadr exp))
           (val (caddr exp))
           (pvar? (equal? 'pvar (car var)))
           (bvar? (equal? 'bvar (car var))))

      (cond  (pvar? (gen-code-set-pvar var val major clist flist ))
                (bvar? (gen-code-set-bvar var val major clist flist ))
                (else (gen-code-set-fvar var val major clist flist )))
      )))

(define gen-code-box
    (lambda (exp major clist flist)
      (let* ((var (cadr exp)))
      (string-append 
        (code-gen var major clist flist) "\n"
        "mov rbx, rax\n"
        "mov rdi, 8 \n"   ;rbx = malloc(8*(m+1)) env
        "call malloc\n"
        "test rax, rax\n"
        "mov qword [rax], rbx\n\n"
         ))))


(define gen-code-box-get
    (lambda (exp major clist flist)
      (let* ((var (cadr exp)))
      (string-append 
        (code-gen var major clist flist) "\n"
        "mov rax, [rax]\n"
         ))))

(define gen-code-box-set
    (lambda (exp major clist flist)
      (let* ((var (cadr exp))
             (val (caddr exp)))
      (string-append 
        (code-gen val major clist flist) "\n"
        "mov rbx, rax\n"
        (code-gen var major clist flist) "\n"
        "mov qword [rax], rbx\n\n"
        "mov rax, SOB_VOID\n\n"
         ))))

(define gen-code-define
    (lambda (exp major clist flist)
      (let* ((var (cadr exp))
      		 (val (caddr exp)))

      (string-append
          (code-gen val major clist flist) "\n"
          "mov [" (find-label flist (cadr var))"], rax \n" 
          "mov  rax , " (find-label clist (void)) "\n"))))


(define gen-code-const
    (lambda (exp clist flist)
      (let* ((val (cadr exp)))
        (symbol? val)
      (string-append "mov rax, " (find-label clist val) "\n" ))))

(define gen-code-fvar
    (lambda ( exp major clist flist)
      (let* ((val (cadr exp)))
      (string-append "mov rax, [" (find-label flist val) "]\n" ))))

(define gen-code-pvar
    (lambda ( exp major clist flist)
      (let* ((val (cadr exp))
             (mi  (caddr exp)))
      (string-append "mov rax, qword [rbp + (4+" (number->string mi) ")*8]" ))))

(define gen-code-bvar
    (lambda ( exp major clist flist)
      (let* ((val (cadr exp))
             (ma  (caddr exp))
             (mi  (cadddr exp)))
      (string-append 
        "mov rax, qword [rbp + 2*8]\n" ;env
        "mov rax, qword [rax + " (number->string  ma) "*8]\n" ;env[ma]
        "mov rax, qword [rax +" (number->string mi) "*8]\n"  ;env[ma][mi]         
      ))))

(define gen-code-if3
    (lambda (exp major clist flist)
        (let* ((dtest (cadr exp))
               (dthen (caddr exp))
               (delse (cadddr exp))
               (tag-else (string-append "else_"(symbol->string (gensym))))
               (tag-end (string-append "end_"(symbol->string (gensym)))))
        (string-append
            (code-gen dtest major clist flist) "\n"
            "mov rax, [rax]\n"
            "cmp rax, SOB_FALSE\n"
            "je " tag-else "\n"
            (code-gen dthen major clist flist) "\n"
            "jmp " tag-end "\n"
            tag-else ":\n"
            (code-gen delse major clist flist) "\n"
            tag-end ":\n"

        ))))

(define gen-code-or
    (lambda (exp major clist flist)
    (let* ((or-label (string-append "end_or_"(symbol->string (gensym))))
             (remove-last (lambda(lst) (reverse (cdr (reverse lst)))))
             (get-last (lambda(lst) (car (reverse lst))))
             (arg-lst (remove-last(cadr exp)))
             (last (get-last(cadr exp)))
             )
       (string-append (fold-left string-append "" 
       	(map (lambda (x)
       		(string-append 
       			(code-gen x major clist flist) "\n" 
            "cmp rax," (find-label const-list #f)"\n"
            "jne " or-label "\n"   )    			

       		) arg-lst))  
       	(code-gen last major clist flist) "\n" 
       	or-label ":\n"
       ))))

(define gen-code-seq 
    (lambda (exp major clist flist)
    (if (null? exp) ""
        (let ((gen-arg (code-gen (car exp) major clist flist)))
        	(string-append gen-arg "\n" (gen-code-seq (cdr exp) major clist flist) )))) )          
           


(define gen-code-applic 
    (lambda (exp major clist flist)
        (let* ((op (cadr exp))
              (args (caddr exp)) 
              (gen-op (code-gen op major clist flist))
              (gen-args (apply string-append (map (lambda (a) 
                                                      (string-append 
                                                        (code-gen a major clist flist) 
                                                        "\npush rax\n")) (reverse args)) ))
              (args-length (length args))
              (inon-label (string-append "inon"(symbol->string (gensym))))
              )
              (string-append
                gen-args "\n";args
                "push " (number->string args-length) "\n" ;number of args
                gen-op "\n"
                "mov rax, [rax]\n"
                "mov rbx,rax\n"
                "CLOSURE_ENV rbx\n"  
                "push rbx\n"                ;env
                "CLOSURE_CODE rax\n"
                "call rax\n"
                "mov rbx, [rsp+1*8]\n"
                "add rbx, 2\n"
                "shl rbx , 3\n"
                "add rsp, rbx\n"
                )
        )
         )) 

(define gen-code-tc-applic 
    (lambda (exp major clist flist)
        (let* ((op (cadr exp))
              (args (caddr exp)) 
              (gen-op (code-gen op major clist flist))
              (gen-args (apply string-append (map (lambda (a) 
                                                      (string-append 
                                                        (code-gen a major clist flist) 
                                                        "\npush rax\n")) (reverse args)) ))
              (args-length (length args))
              (inon-label (string-append "inon"(symbol->string (gensym))))
              (repair-stack1 (string-append "repair_stack_bottom_to_top"(symbol->string (gensym))))
              )
              (string-append
                gen-args "\n";args
                "push " (number->string args-length) "\n" ;number of args
                gen-op "\n"
                "mov rax, [rax]\n"
                "mov rbx,rax\n"
                "CLOSURE_ENV rbx\n"  
                "push rbx\n"                ;env
                "CLOSURE_CODE rax\n"
                "push qword [rbp+8]\n"      ; get ret from top
                "mov r13,"(number->string args-length) "\n"
                "add r13, 1\n"  ;length
                "add r13, 2\n"  ;env+ret
                "mov r14, rbp\n"
                "sub r14, 8\n"  ; point to the first arg
                "mov r12 , [rbp+3*8]\n"
                "add r12 , 3\n"
                "shl r12, 3\n"   
                "add r12, rbp\n"
                "mov r15 , r12\n"
                "mov rbp, [rbp]\n"
                repair-stack1":\n"
                "mov r11,[r14]\n"              ;r11 <- the value to move
                "mov qword [r15], r11\n"
                "sub r14,8\n"
                "sub r15,8\n"
                "dec r13\n"
                "cmp r13,0\n"
                "ja "repair-stack1"\n" 
                "sub r15, r14\n" 
                "add rsp , r15\n"

                "jmp rax\n"
                )
        )
         ))

(define gen-code-lambda-opt
  (lambda(exp major clist flist)
    (let* ((lambda-label1 (string-append "loop1_start"(symbol->string (gensym))))
           (lambda-label2 (string-append "loop2_start"(symbol->string (gensym))))
           (lambda-label3 (string-append "code_start"(symbol->string (gensym))))
           (lambda-label4 (string-append "code_end"(symbol->string (gensym))))
            (opt-loop (string-append "opt_loop"(symbol->string (gensym))))
            (repair-stack (string-append "repair_stack"(symbol->string (gensym))))
            (sub-size-r10 (string-append "sub_size_r10_bigger"(symbol->string (gensym))))
            (sub-size-c (string-append "sub_size_continue"(symbol->string (gensym))))
            (no-opt-params (string-append "no_opt_params"(symbol->string (gensym))))
            (inon-label (string-append "inon"(symbol->string (gensym))))
            (repair-stack1 (string-append "repair_stack_bottom_to_top"(symbol->string (gensym))))
            (repair-stack2 (string-append "repair_stack_top_to_bottom"(symbol->string (gensym))))
            (no-opt-params (string-append "no_opt_params"(symbol->string (gensym))))
            (cont (string-append "cont"(symbol->string (gensym))))
           (params (cadr exp))
           (opt (caddr exp))
           (body (cadddr exp))
           (params-length (number->string (length params))))    ;the string n

      (string-append  "\n"
      inon-label ":\n"
      ; "mov rbp,rsp\n"
      (if (= 0 major) "xor rbx,rbx\n"
      (string-append
      "mov rdi, 8*" (number->string (+ 1 major)) "\n"   ;rbx = malloc(8*(m+1)) env
      "push rcx\n"
      "push r8\n"
      "push r9\n"
      "push r10\n"
      "push r11\n"
      "push r12\n"

      "call malloc\n"
      "test rax, rax\n"
      "pop r12\n"
      "pop r11\n"
      "pop r10\n"
      "pop r9\n"
      "pop r8\n"
      "pop rcx\n"

      "mov rbx, rax\n\n"))

      (if (= 0 major) ""
      (string-append
      "mov rdi, [rbp+3*8]\n" ; jmp to length of parent args
      "push rdi\n"           ; push number of args of parent
      "shl rdi, 3\n"    ;r8 = malloc(8*n)     ;r8=rcx in the psuado code
      "call malloc\n"
      "test rax, rax\n"
      "mov r8, rax\n\n"))

      "mov rdi, 16\n"    ;rax = malloc(8)
      "push rcx\n"
      "push r8\n"
      "push r9\n"
      "push r10\n"
      "push r11\n"
      "push r12\n"

      "call malloc\n"
      "test rax, rax\n"
      "pop r12\n"
      "pop r11\n"
      "pop r10\n"
      "pop r9\n"
      "pop r8\n"
      "pop rcx\n"

      (if (= 0 major) ""
      (string-append
      "pop rdi\n"
      "mov r10,0\n"             ;for r10 from 0 to n do : r8[r10]=param[r10]
      lambda-label1":\n"
      "mov r11,qword[rbp+8*(r10 + 4)]\n"
      "mov [r8+r10*8],r11\n"
      "inc r10\n"
      "cmp r10,rdi\n"
      "jl "lambda-label1"\n\n"

      "mov qword [rbx],r8\n"))    ;rbx[0]=r8

      (if (> 2 major) ""
      (string-append
      "mov r9,1\n"
      "mov r10,0\n"             ;for r10 from 0 to n do : r8[r10]=param[r10]
      lambda-label2":\n"
      "mov rsi, qword[rbp+8*2]\n"     ;rsi = env
      "mov rsi,[rsi+r10*8]\n"     ;rsi=env[r10]
      "mov [rbx+r9*8],rsi\n"      ;rbx[r9] = rsi;
      "inc r10\n"
      "inc r9\n"
      "cmp r9," (number->string major) "\n"
      "jl "lambda-label2"\n\n"))

      "mov r12, "params-length "\n"
      "MAKE_LITERAL_CLOSURE rax , rbx , " lambda-label3 "\n"
      "jmp " lambda-label4"\n\n"
      lambda-label3":\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      
      "mov r14, [rbp +3*8]\n"    ; r114<- number of params on stack
      "mov r13, r14\n"            
      "add r13, 3\n"
      "shl r13, 3\n"            
      "mov r12, rbp\n"        
      "add r12, r13\n"           ; r12 <- point to the last arg 
      "sub r14, "params-length"\n"      ; r14 <- number of opt args
      "cmp r14,0\n"
      "je "no-opt-params "\n"
      "mov r15 , 0\n"
      "mov r13 , ["(find-label const-list '())"]\n"
      opt-loop ":\n"
      

      "mov rdi, 16\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov r11, [r12]\n"
      "mov r11, [r11]\n"
      "mov qword[rax] , r11\n"
      "mov qword[rax+8] , r13\n"
      "mov r8, rax\n"
      "add rax, 8\n"
      "mov r13, rax\n"


      "MAKE_LITERAL_PAIR2 r8, r13\n"

      "mov r13, r8\n"
      
      "inc r15\n"
      "sub r12, 1*8\n"
      "cmp r15,r14\n"
      "jl "opt-loop "\n"

      "mov rdi, 8\n"
      "push rcx\n"
      "push r8\n"
      "push r9\n"
      "push r10\n"
      "push r11\n"
      "push r12\n"

      "call malloc\n"
      "test rax, rax\n"
      "pop r12\n"
      "pop r11\n"
      "pop r10\n"
      "pop r9\n"
      "pop r8\n"
      "pop rcx\n"
      "mov qword [rax], r13\n"

      "add r12 , 1*8\n"   ; r12 point to the palce to add the list


      "mov qword [r12], rax \n"

      "mov r9 , 1\n"
      "add r9 , "params-length "\n"     ; number of new params 


      "mov r14, r12\n"
      "mov r12, [rbp+ 3*8]\n"
      "mov qword [rbp +3*8] , r9 \n"

      "mov r15, r12\n" 
      "add r15, 3\n"
      "shl r15, 3\n"
      "add r15, rbp\n"

      "mov r13 , r14\n"
      "sub r13 , rbp\n"
      "shr r13 , 3\n"
      "add r13, 1\n"

repair-stack1":\n"
"mov r11,[r14]\n"              ;r11 <- the value to move
"mov qword [r15], r11\n"
"sub r14,8\n"
"sub r15,8\n"
"dec r13\n"
"cmp r13,0\n"
"ja "repair-stack1"\n" 

"sub r15, r14\n" 
"add rsp , r15\n"
"add rbp , r15\n"
"jmp "cont"\n"

      no-opt-params ":\n"
      "mov r13, [rbp +3*8]\n"    ; r114<- number of params on stack
      "mov r12, r13\n"    ; r114<- number of params on stack
      "inc r12\n"
      "mov qword [rbp+3*8] , r12\n"
      "add r13, 3\n"
      "add r13, 1\n"
      "mov r14, rbp\n"
      "mov r15, r14\n"
      "sub r15 , 8\n"
      repair-stack2":\n"
      "mov r11,[r14]\n"              ;r11 <- the value to move
      "mov qword [r15], r11\n"
      "add r14,8\n"
      "add r15,8\n"
      "dec r13\n"
      "cmp r13,0\n"
      "ja "repair-stack2"\n" 
      "sub rsp , 8\n"
      "sub rbp , 8\n"
      "mov r14,"(find-label const-list '())"\n"
      "mov qword [r15], r14\n"

      cont":\n"
      (code-gen body (+ 1 major) clist flist) "\n"
      ; [[body]]
      "pop rbp\n"
      "ret\n"
      lambda-label4":\n"
      
      ))
    ))

(define gen-code-lambda-simple
  (lambda(exp major clist flist)
    (let* ((lambda-label1 (string-append "loop1_start"(symbol->string (gensym))))
           (lambda-label2 (string-append "loop2_start"(symbol->string (gensym))))
           (lambda-label3 (string-append "code_start"(symbol->string (gensym))))
           (lambda-label4 (string-append "code_end"(symbol->string (gensym))))

           (inon-label (string-append "inon"(symbol->string (gensym))))

           (params (cadr exp))
           (body (caddr exp))
           (params-length (number->string (length params))))    ;the string n
      (string-append  "\n"
      inon-label ":\n"
      (if (= 0 major) "xor rbx,rbx\n"
      (string-append
      "mov rdi, 8*" (number->string (+ 1 major)) "\n"   ;rbx = malloc(8*(m+1)) env
      "push rcx\n"
      "push r8\n"
      "push r9\n"
      "push r10\n"
      "push r11\n"
      "push r12\n"

      "call malloc\n"
      "test rax, rax\n"
      "pop r12\n"
      "pop r11\n"
      "pop r10\n"
      "pop r9\n"
      "pop r8\n"
      "pop rcx\n"
      "mov rbx, rax\n\n"))

      (if (= 0 major) ""
      (string-append
      "mov rdi, [rbp+3*8]\n" ; jmp to length of parent args
      "push rdi\n"           ; push number of args of parent
      "shl rdi, 3\n"    ;r8 = malloc(8*n)     ;r8=rcx in the psuado code
       "push rcx\n"
      "push r8\n"
      "push r9\n"
      "push r10\n"
      "push r11\n"
      "push r12\n"

      "call malloc\n"
      "test rax, rax\n"
      "pop r12\n"
      "pop r11\n"
      "pop r10\n"
      "pop r9\n"
      "pop r8\n"
      "pop rcx\n"
      "mov r8, rax\n\n"))

      "mov rdi, 16\n"    ;rax = malloc(8)
      "push rcx\n"
      "push r8\n"
      "push r9\n"
      "push r10\n"
      "push r11\n"
      "push r12\n"

      "call malloc\n"
      "test rax, rax\n"
      "pop r12\n"
      "pop r11\n"
      "pop r10\n"
      "pop r9\n"
      "pop r8\n"
      "pop rcx\n"

      (if (= 0 major) ""
      (string-append
      "pop rdi\n"
      "mov r10,0\n"             ;for r10 from 0 to n do : r8[r10]=param[r10]
      lambda-label1":\n"
      "mov r11,qword[rbp+8*(r10 + 4)]\n"
      "mov [r8+r10*8],r11\n"
      "inc r10\n"
      "cmp r10,rdi\n"
      "jl "lambda-label1"\n\n"

      "mov qword [rbx],r8\n"))    ;rbx[0]=r8

      (if (> 2 major) ""
      (string-append
      "mov r9,1\n"
      "mov r10,0\n"             ;for r10 from 0 to n do : r8[r10]=param[r10]
      lambda-label2":\n"
      "mov rsi, qword[rbp+8*2]\n"     ;rsi = env
      "mov rsi,[rsi+r10*8]\n"     ;rsi=env[r10]
      "mov [rbx+r9*8],rsi\n"      ;rbx[r9] = rsi;
      "inc r10\n"
      "inc r9\n"
      "cmp r9," (number->string major) "\n"
      "jl "lambda-label2"\n\n"))

      "mov r12, "params-length "\n"
      "MAKE_LITERAL_CLOSURE rax , rbx , " lambda-label3 "\n"
      "jmp " lambda-label4"\n\n"
      lambda-label3":\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      

      (code-gen body (+ 1 major) clist flist) "\n"
      ; [[body]]
      "pop rbp\n"
      "ret\n"
      lambda-label4":\n"
    ))
    ))


(define my-reverse
	(lambda (lst)
  		(if (null? lst)
     	'()
    	 (append (reverse (cdr lst)) (list (car lst))))
  )
)

(define impl-apply
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, apply_code\n"
      "jmp apply_end\n"

      "apply_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r13,[rbp+8*(4+1)]\n" 
      "mov r11,r13\n"
      "mov r13,[r13]\n"         ;list of args
      "mov r14,[rbp+8*(4+0)]\n" 
      "mov r14,[r14]\n"         ;func
      "mov r15,[rbp+8*(3+0)]\n" ;arg count
      ;---------------------------------------------------
      
      "push r11\n"
      "push r12\n"
      "push r13\n"  ;saving before doing call to gdc
      "push r14\n"
      "push r15\n"


      "push r11\n"         
      "push 1\n"
      "mov rax, ["(find-label fvar-list 'meirlength)"]\n"
      "mov rax ,[rax]\n"
      "mov rbx,rax\n"
      "CLOSURE_ENV rbx\n"
      "push rbx\n"
      "CLOSURE_CODE rax\n"
      "call rax\n"
      "add rsp,3*8\n"   
      "pop r15\n"
      "pop r14\n"
      "pop r13\n"
      "pop r12\n"
      "pop r11\n"

      "after_length:\n"

      ;;;;;;;;;;;;;;;;;;;;;;; rax hold the length of the list
      "mov rax , [rax]\n"
      "DATA_LOWER rax\n"

      "cmp rax, 0\n"              ; check if length is 0 => '()
      "je no_args_to_apply\n"
      "mov r12, rax\n"
      "shl rax, 3\n"
      "mov r10 , rsp\n"
      "sub r10, rax \n"           ; r10 point to the loc where to put the last arg ( the first in the list)
      "sub r10,8\n"
      "mov qword [r10], r12\n"
      "add r10,8\n"
      "mov r15, r13\n"
      "push_loop:\n "
      "mov r11, r15\n"
      "TRY r11\n"

      "mov qword [r10] , r11\n"
      "CDR r15\n"
      "add r10, 8\n"
      "cmp r15 , SOB_NIL\n"
      "jne push_loop\n"

      "mov r10, r12\n"
      "shl r12, 3\n"
      "sub rsp , r12\n"
      "sub rsp, 8\n"
      "jmp apply_continue\n"


      "no_args_to_apply:\n"
      "push 0\n"

              "apply_continue:\n"
                "mov rax, r14\n"
                "mov rbx,rax\n"
                "CLOSURE_ENV rbx\n"  
                "push rbx\n"                ;env
                "CLOSURE_CODE rax\n"
                "push qword [rbp+8]\n"      ; get ret from top
                "mov r13,r10\n"
                "add r13, 1\n"  ;length
                "add r13, 2\n"  ;env+ret
                "mov r14, rbp\n"
                "sub r14, 8\n"  ; point to the first arg
                "mov r12 , [rbp+3*8]\n"
                "add r12 , 3\n"
                "shl r12, 3\n"   
                "add r12, rbp\n"
                "mov r15 , r12\n"
                "mov rbp, [rbp]\n"
                "repair_stack:\n"
                "mov r11,[r14]\n"              ;r11 <- the value to move
                "mov qword [r15], r11\n"
                "sub r14,8\n"
                "sub r15,8\n"
                "dec r13\n"
                "cmp r13,0\n"
                "ja repair_stack\n" 
                "sub r15, r14\n" 
                "add rsp , r15\n"
                ; "add rbp , r15\n"


                "jmp rax\n"      

      ;;do reverse with r13 and '()
      ;;loop over the list and push list args
      ;;call the func 


      "apply_finish:\n"

      "leave\n"
      "ret\n"

      "apply_end:\n"
      "mov [" (find-label fvar-list 'apply) "], rax\n\n")))


(define impl-not
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, not_code\n"
      "jmp not_end\n"

      "not_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "cmp r14,SOB_FALSE\n"
      "je return_true\n"
      "mov rax," (find-label const-list #f)"\n"
      "jmp not_finish\n"
      "return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "not_finish:\n"

      "leave\n"
      "ret\n"

      "not_end:\n"
      "mov [" (find-label fvar-list 'not) "], rax\n\n")))

(define impl-car
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, car_code\n"
      "jmp car_end\n"

      "car_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "TRY r14\n"
      "mov rax,r14\n"
      "car_finish:\n"

      "mov r14,rax\n"
      "leave\n"
      "ret\n"

      "car_end:\n"
      "mov [" (find-label fvar-list 'car) "], rax\n\n"))) 

(define impl-string-len
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, string_len_code\n"
      "jmp string_len_end\n"

      "string_len_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "STRING_LENGTH r14\n"
      "mov rax,r14\n"
      "string_len_finish:\n"

      "shl rax,4\n"  
      "add rax, T_INTEGER\n"      

      "mov r14,rax\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"

      "leave\n"
      "ret\n"

      "string_len_end:\n"
      "mov [" (find-label fvar-list 'string-length) "], rax\n\n")))

(define impl-vector-len
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, vector_len_code\n"
      "jmp vector_len_end\n"

      "vector_len_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "VECTOR_LENGTH r14\n"
      "mov rax,r14\n"
      "vector_len_finish:\n"

      "shl rax,4\n" 
      "add rax, T_INTEGER\n"      

      "mov r14,rax\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"

      "leave\n"
      "ret\n"

      "vector_len_end:\n"
      "mov [" (find-label fvar-list 'vector-length) "], rax\n\n")))

(define impl-string-ref
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, string_ref_code\n"
      "jmp string_ref_end\n"

      "string_ref_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"

      "mov r13,[rbp+8*(5+0)]\n" ;arg location
      "mov r13,[r13]\n"     
      "DATA_LOWER r13\n"
      "mov r14,[rbp+8*(4+0)]\n" ;string
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rbx,0\n"
      "STRING_REF bl, r14, r13\n"

      "shl rbx,4\n"  
      "add rbx, T_CHAR\n"      

      "string_ref_finish:\n"

      "mov r14,rbx\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"

      "leave\n"
      "ret\n"

      "string_ref_end:\n"
      "mov [" (find-label fvar-list 'string-ref) "], rax\n\n")))

(define impl-vector-ref
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, vector_ref_code\n"
      "jmp vector_ref_end\n"

      "vector_ref_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
    
      "mov r13,[rbp+8*(5+0)]\n" ;arg location
      "mov r13,[r13]\n"     
      "DATA_LOWER r13\n"
      "mov r14,[rbp+8*(4+0)]\n" ;vector
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rbx,0\n"
      "VECTOR_REF2 rbx, r14, r13\n"
      "mov rax , rbx\n"

      "leave\n"
      "ret\n"

      "vector_ref_end:\n"
      "mov [" (find-label fvar-list 'vector-ref) "], rax\n\n")))

(define impl-cdr
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, cdr_code\n"
      "jmp cdr_end\n"

      "cdr_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "TRY2 r14\n"
      "mov rax,r14\n"
      "cdr_finish:\n"
      "leave\n"
      "ret\n"

      "cdr_end:\n"
      "mov [" (find-label fvar-list 'cdr) "], rax\n\n")))

(define impl-is-zero
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_zero_code\n"
      "jmp is_zero_end\n"

      "is_zero_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "DATA_LOWER r14\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "cmp r14,0\n"
      "je is_zero_return_true\n"
      "mov rax," (find-label const-list #f)"\n"
      "jmp is_zero_finish\n"
      "is_zero_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_zero_finish:\n"

      "leave\n"
      "ret\n"

      "is_zero_end:\n"
      "mov [" (find-label fvar-list 'zero?) "], rax\n\n")))

(define impl-is-integer
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_integer_code\n"
      "jmp is_integer_end\n"

      "is_integer_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "TYPE r14\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "cmp r14,T_INTEGER\n"
      "je is_integer_return_true\n"
      "mov rax," (find-label const-list #f)"\n"
      "jmp is_zero_finish\n"
      "is_integer_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_integer_finish:\n"

      "leave\n"
      "ret\n"

      "is_integer_end:\n"
      "mov [" (find-label fvar-list 'integer?) "], rax\n\n")))

(define impl-is-string
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_string_code\n"
      "jmp is_string_end\n"

      "is_string_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rax," (find-label const-list #f)"\n"
      "mov rbx,r14\n"
      "TYPE rbx\n"
      "cmp rbx,T_STRING\n"
      "je is_string_return_true\n"
      
      "jmp is_string_finish\n"

      "is_string_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_string_finish:\n"

      "leave\n"
      "ret\n"

      "is_string_end:\n"
      "mov [" (find-label fvar-list 'string?) "], rax\n\n")))

(define impl-is-symbol
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_symbol_code\n"
      "jmp is_symbol_end\n"

      "is_symbol_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rax," (find-label const-list #f)"\n"
      "mov rbx,r14\n"
      "TYPE rbx\n"
      "cmp rbx,T_SYMBOL\n"
      "je is_symbol_return_true\n"
      
      "jmp is_symbol_finish\n"

      "is_symbol_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_symbol_finish:\n"

      "leave\n"
      "ret\n"

      "is_symbol_end:\n"
      "mov [" (find-label fvar-list 'symbol?) "], rax\n\n")))

(define impl-is-proc
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_proc_code\n"
      "jmp is_proc_end\n"

      "is_proc_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rax," (find-label const-list #f)"\n"
      "mov rbx,r14\n"
      "TYPE rbx\n"
      "cmp rbx,T_CLOSURE\n"
      "je is_proc_return_true\n"
      
      "jmp is_proc_finish\n"

      "is_proc_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_proc_finish:\n"

      "leave\n"
      "ret\n"

      "is_proc_end:\n"
      "mov [" (find-label fvar-list 'procedure?) "], rax\n\n")))

(define impl-is-vector
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_vector_code\n"
      "jmp is_vector_end\n"

      "is_vector_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rax," (find-label const-list #f)"\n"
      "mov rbx,r14\n"
      "TYPE rbx\n"
      "cmp rbx,T_VECTOR\n"
      "je is_vector_return_true\n"
      
      "jmp is_vector_finish\n"

      "is_vector_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_vector_finish:\n"

      "leave\n"
      "ret\n"

      "is_vector_end:\n"
      "mov [" (find-label fvar-list 'vector?) "], rax\n\n")))

(define impl-is-boolean
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_boolean_code\n"
      "jmp is_boolean_end\n"

      "is_boolean_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rax," (find-label const-list #f)"\n"
      "mov rbx,r14\n"
      "TYPE rbx\n"
      "cmp rbx,T_BOOL\n"
      "je is_boolean_return_true\n"
      
      "jmp is_boolean_finish\n"

      "is_boolean_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_boolean_finish:\n"

      "leave\n"
      "ret\n"

      "is_boolean_end:\n"
      "mov [" (find-label fvar-list 'boolean?) "], rax\n\n")))

(define impl-is-pair
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_pair_code\n"
      "jmp is_pair_end\n"

      "is_pair_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rax," (find-label const-list #f)"\n"
      "mov rbx,r14\n"
      "TYPE rbx\n"
      "cmp rbx,T_PAIR\n"
      "je is_pair_return_true\n"
      
      "jmp is_pair_finish\n"

      "is_pair_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_pair_finish:\n"

      "leave\n"
      "ret\n"

      "is_pair_end:\n"
      "mov [" (find-label fvar-list 'pair?) "], rax\n\n")))

(define impl-is-char
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_char_code\n"
      "jmp is_char_end\n"

      "is_char_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rax," (find-label const-list #f)"\n"
      "mov rbx,r14\n"
      "TYPE rbx\n"
      "cmp rbx,T_CHAR\n"
      "je is_char_return_true\n"
      
      "jmp is_char_finish\n"

      "is_char_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_char_finish:\n"
      "leave\n"
      "ret\n"

      "is_char_end:\n"
      "mov [" (find-label fvar-list 'char?) "], rax\n\n")))

(define impl-is-null
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_null_code\n"
      "jmp is_null_end\n"

      "is_null_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rax," (find-label const-list #f)"\n"
      "mov rbx,r14\n"
      "TYPE rbx\n"
      "cmp rbx,T_NIL\n"
      "je is_null_return_true\n"
      
      "jmp is_null_finish\n"

      "is_null_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_null_finish:\n"

      "leave\n"
      "ret\n"

      "is_null_end:\n"
      "mov [" (find-label fvar-list 'null?) "], rax\n\n")))

(define impl-is-num
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_num_code\n"
      "jmp is_num_end\n"

      "is_num_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rax," (find-label const-list #f)"\n"
      "mov rbx,r14\n"
      "TYPE rbx\n"
      "cmp rbx,T_INTEGER\n"
      "je is_num_return_true\n"
      
      "cmp rbx,T_FRACTION\n"
      "je is_num_return_true\n"

      "jmp is_num_finish\n"

      "is_num_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_num_finish:\n"

      "leave\n"
      "ret\n"

      "is_num_end:\n"
      "mov [" (find-label fvar-list 'number?) "], rax\n\n")))

(define impl-is-rational
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_rational_code\n"
      "jmp is_rational_end\n"

      "is_rational_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rax," (find-label const-list #f)"\n"
      "mov rbx,r14\n"
      "TYPE rbx\n"
      "cmp rbx,T_INTEGER\n"
      "je is_rational_return_true\n"
      
      "cmp rbx,T_FRACTION\n"
      "je is_rational_return_true\n"

      "jmp is_rational_finish\n"

      "is_rational_return_true:\n"
      "mov rax," (find-label const-list #t)"\n"

      "is_rational_finish:\n"

      "leave\n"
      "ret\n"

      "is_rational_end:\n"
      "mov [" (find-label fvar-list 'rational?) "], rax\n\n")))

(define impl-remainder
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, remainder_code\n"
      "jmp remainder_end\n"

      "remainder_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r13,[rbp+8*(5+0)]\n" ;arg 1 diviser
      "mov r13,[r13]\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg 0 dividend
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov r8, r13\n"
      "mov r9, r14\n"

      "DATA_LOWER r8\n" ;DATA_LOWER = the actual numbers without the type
      "DATA_LOWER r9\n"

      "xor rdx, rdx \n"
      "mov rax, r9\n"
      "cqo\n"
      "idiv r8\n"
      "mov rax, rdx \n" ;rax holds remainder value
      "TYPE r13\n"
      "shl rax,4\n"
      "add rax, r13\n"  ;rax hold remainder and of type INTEGER

      "remainder_finish:\n"
      "mov r14,rax\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"


      "leave\n"
      "ret\n"

      "remainder_end:\n"
      "mov [" (find-label fvar-list 'remainder) "], rax\n\n")))

(define impl-set-car!
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, set_car_code\n"
      "jmp set_car_end\n"

      "set_car_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r13,[rbp+8*(5+0)]\n" ;arg to change
      "mov r14,[rbp+8*(4+0)]\n" ;the list
      "mov r15,[rbp+8*(3+0)]\n" ;arg count
    
      "mov r13,[r13]\n" ;r13<-the new car

      "mov r12,[r14]\n"
      "CDR r12\n" ;r14<-the new cdr

      "mov rdi, 16\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r13\n"
      "mov qword[rax+8] , r12\n"
      "mov r8, rax\n"
      "add rax, 8\n"
      "mov r13, rax\n"

      "MAKE_LITERAL_PAIR2 r8 , r13\n"
      "mov [r14],r8\n"  ;r14<-the new pair with new car and new cdr

      "mov rax," (find-label const-list (void))"\n"

      "set_car_finish:\n"
      "leave\n"
      "ret\n"

      "set_car_end:\n"
      "mov [" (find-label fvar-list 'set-car!) "], rax\n\n")))

(define impl-set-cdr!
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, set_cdr_code\n"
      "jmp set_cdr_end\n"

      "set_cdr_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r13,[rbp+8*(5+0)]\n" ;arg to change
      "mov r14,[rbp+8*(4+0)]\n" ;the list
      "mov r15,[rbp+8*(3+0)]\n" ;arg count
    
      "mov r13,[r13]\n" ;r13<-the new cdr

      "mov r12,[r14]\n"
      "CAR r12\n" ;r14<-the new car

      "mov rdi, 16\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r12\n"
      "mov qword[rax+8] , r13\n"
      "mov r8, rax\n"
      "add rax, 8\n"
      "mov r13, rax\n"

      "MAKE_LITERAL_PAIR2 r8 , r13\n"
      "mov [r14],r8\n"  ;r14<-the new pair with new car and new cdr

      "mov rax," (find-label const-list (void))"\n"

      "set_cdr_finish:\n"
      "leave\n"
      "ret\n"

      "set_cdr_end:\n"
      "mov [" (find-label fvar-list 'set-cdr!) "], rax\n\n")))

(define impl-make-string 
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, make_string_code\n"
      "jmp make_string_end\n"

      "make_string_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"

      "mov r14,[rbp+8*(4+0)]\n" ;length of string
      "mov r14,[r14]\n"
      "DATA_LOWER r14\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "cmp r15,1\n"
      "je make_string_new_obj\n"
      "mov r13,[rbp+8*(5+0)]\n" ;the object
      "mov r13,[r13]\n"
      "DATA_LOWER r13\n"
      "jmp make_string_after\n"

      "make_string_new_obj:\n"
      "mov r13,0 \n"            ;fixxxxxxxxx!!

      "make_string_after:\n"
      "mov rdi, r14\n"
      "shl rdi,3\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov r8,rax\n"  ;r8,rax hold a pointer to malloc location
      "mov r12,0\n"

      "make_string_loop_start:\n" ;loop start

      "cmp r12,r14\n"
      "je make_string_loop_end\n"
      "mov [r8+r12],r13\n"  ;fill r8,rax with r14 chars 
      "inc r12\n"
      "jmp make_string_loop_start\n"

      "make_string_loop_end:\n" ;loop end

      "MAKE_LITERAL_STRING2 rax, r14\n"

      "make_string_finish:\n"
      "mov r14,rax\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"
      "leave\n"
      "ret\n"

      "make_string_end:\n"
      "mov [" (find-label fvar-list 'make-string) "], rax\n\n")))

(define impl-string-set!
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, string_set_code\n"
      "jmp string_set_end\n"

      "string_set_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"

      "mov r11,[rbp+8*(6+0)]\n" 
      "mov r11,[r11]\n"
      "DATA_LOWER r11\n"  ;r11 = char

      "mov r13,[rbp+8*(5+0)]\n" 
      "mov r13,[r13]\n"
      "DATA_LOWER r13\n"  ;r13 = location

      "mov r14,[rbp+8*(4+0)]\n" 
      "mov r14,[r14]\n"
      "mov r10,r14\n" ;r10 = pointer to string
      "mov r15,[rbp+8*(3+0)]\n" ;r15 = arg count
      "STRING_LENGTH r14\n" ;r14 = length of string

      "mov rdi, r14\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov r8,rax\n"  ;r8,rax hold a pointer to malloc location
      "mov r12,0\n"

      "string_set_loop_start:\n" ;loop start

      "cmp r12,r14\n"
      "je string_set_loop_end\n"

      "cmp r12,r13\n"
      "je string_set_change\n"

      "mov rbx,0\n"
      "STRING_REF bl, r10, r12\n"

      "mov [r8+r12],rbx\n"  ;fill r8/rax with  char from string
      "jmp string_set_continue_loop\n"
      "string_set_change:\n"
      "mov [r8+r12],r11\n"  ;fill r8/rax with the char
      "string_set_continue_loop:"
      "inc r12\n"
      "jmp string_set_loop_start\n"

      "string_set_loop_end:\n" ;loop end

      "MAKE_LITERAL_STRING2 rax, r14\n"

      "string_set_finish:\n"
       "mov r14,[rbp+8*(4+0)]\n" 
       "mov [r14],rax\n"
      "mov rax," (find-label const-list (void))"\n"
      "leave\n"
      "ret\n"

      "string_set_end:\n"
      "mov [" (find-label fvar-list 'string-set!) "], rax\n\n")))

(define impl-make-vector 
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, make_vector_code\n"
      "jmp make_vector_end\n"

      "make_vector_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"

      "mov r14,[rbp+8*(4+0)]\n" ;length of vector
      "mov r14,[r14]\n"
      "DATA_LOWER r14\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "cmp r15,1\n"
      "je make_vector_new_obj\n"
      "mov r13,[rbp+8*(5+0)]\n" ;the object
      "jmp make_vector_after\n"

      "make_vector_new_obj:\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov r13,0 \n"
      "shl r13,4\n"
      "add r13,T_INTEGER\n"
      "mov [rax],r13\n"
      "mov r13,rax\n"

      "make_vector_after:\n"
      "mov rax,8\n"
      "mul r14\n"
      "mov r14,rax\n"
      "mov rdi, rax\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov r8,rax\n"  ;r8,rax hold a pointer to malloc location
      "mov r12,0\n"

      "make_vector_loop_start:\n" ;loop start

      "cmp r12,r14\n"
      "je make_vector_loop_end\n"
      "mov [r8+r12],r13\n"  ;fill r8,rax with r14 chars 
      "add r12, 8\n"
      "jmp make_vector_loop_start\n"

      "make_vector_loop_end:\n" ;loop end

      "MAKE_LITERAL_VECTOR2 rax, r14\n"

      "make_vector_finish:\n"
      "mov r14,rax\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"
      "leave\n"
      "ret\n"

      "make_vector_end:\n"
      "mov [" (find-label fvar-list 'make-vector) "], rax\n\n")))

(define impl-vector-set!
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, vector_set_code\n"
      "jmp vector_set_end\n"

      "vector_set_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"

      "mov r11,[rbp+8*(6+0)]\n" 

      "mov r13,[rbp+8*(5+0)]\n" 
      "mov r13,[r13]\n"
      "DATA_LOWER r13\n"  
      "mov rax,8\n"
      "mul r13\n"
      "mov r13,rax\n" ;r13 = location

      "mov r14,[rbp+8*(4+0)]\n" 
      "mov r14,[r14]\n"
      "mov r10,r14\n" ;r10 = pointer to string
      "mov r15,[rbp+8*(3+0)]\n" ;r15 = arg count
      "VECTOR_LENGTH r14\n" ;r14 = length of string

      "mov rax,8\n"
      "mul r14\n"
      "mov r14,rax\n"
      "mov rdi, r14\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov r8,rax\n"  ;r8,rax hold a pointer to malloc location
      "mov r12,0\n"
      "mov r9,0\n"

      "vector_set_loop_start:\n" ;loop start

      "cmp r12,r14\n"
      "je vector_set_loop_end\n"

      "cmp r12,r13\n"
      "je vector_set_change\n"

      "mov rbx,0\n"
      "VECTOR_REF2 rbx, r10, r9\n"

      "mov [r8+r12],rbx\n"  ;fill r8/rax with  char from string
      "jmp vector_set_continue_loop\n"
      "vector_set_change:\n"
      "mov [r8+r12],r11\n"  ;fill r8/rax with the char
      "vector_set_continue_loop:"
      "add r12,8\n"
      "inc r9\n"
      "jmp vector_set_loop_start\n"

      "vector_set_loop_end:\n" ;loop end

      "MAKE_LITERAL_VECTOR2 r8, r14\n"

      "vector_set_finish:\n"
      "mov r14,[rbp+8*(4+0)]\n" 
      "mov [r14],r8\n"
      "mov rax," (find-label const-list (void))"\n"
      "leave\n"
      "ret\n"

      "vector_set_end:\n"
      ;"mov rax,[rax]\n"
      "mov [" (find-label fvar-list 'vector-set!) "], rax\n\n")))

(define impl-vector 
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, vector_code\n"
      "jmp vector_end\n"

      "vector_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"

      "mov r15,[rbp+8*(3+0)]\n" ;arg count
      "mov r14,r15\n"

      "vector_after:\n"
      "shl r14,3\n"
      "mov rdi, r14\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov r8,rax\n"  ;r8,rax hold a pointer to malloc location
      "mov r12,0\n"
      "mov r11,0\n"
      "vector_loop_start:\n" ;loop start

      "cmp r12,r14\n"
      "je vector_loop_end\n"
      "mov r13,[rbp+8*(4+r11)]\n" ;
      "mov r13, [r13]\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "mov [rax], r13\n"
      "mov [r8+r12],rax\n"  ;fill r8, with r13 chars 
      "add r12, 8\n"
      "inc r11\n"

      "jmp vector_loop_start\n"

      "vector_loop_end:\n" ;loop end
      "MAKE_LITERAL_VECTOR2 r8, r14\n"

      "vector_finish:\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r8\n"
      "leave\n"
      "ret\n"

      "vector_end:\n"
      "mov [" (find-label fvar-list 'vector) "], rax\n\n")))

(define impl-smaller
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, less_code\n"
      "jmp less_end\n"

      "less_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r12,0\n"
      "less_loop:\n"
      "mov r13,[rbp+8*(5+r12)]\n" ;arg
      "mov r13,[r13]\n"
      "mov r14,[rbp+8*(4+r12)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "cmp r15,1\n"
      "je less_true\n"

      "mov rdx,r14\n"
      "mov rcx,r13\n"
      "mov r8,1\n"
      "mov r9,1\n"
      "mov r10,r13\n"
      "mov r11,r14\n"
      "TYPE r10\n"
      "TYPE r11\n"
      "cmp r10, T_FRACTION\n"
      "je less_splitNum2\n"

      "less_second:\n"
      "cmp r11, T_FRACTION\n"
      "je less_splitNum1\n"
      "jmp less_after\n"

      "less_splitNum2:\n"
      "DENOMINATOR rcx\n"
      "mov r8,rax\n"      ;r8 hold the DENOMINATOR of r13
      "DATA_LOWER r8\n"
      "NUMERATOR r13\n"
      "mov r13,rax\n"
      "jmp less_second\n"

      "less_splitNum1:\n"
      "DENOMINATOR rdx\n"
      "mov r9,rax\n"      ;r9 hold the DENOMINATOR of r14
      "DATA_LOWER r9\n"
      "NUMERATOR r14\n"
      "mov r14,rax\n"

      "less_after:\n"

      "DATA_LOWER r14\n"
      "mov rax,r8\n"
      "mul r14\n"
      "mov r14,rax\n"

      "DATA_LOWER r13\n"
      "mov rax,r9\n"
      "mul r13\n"
      "mov r13,rax\n"

      "cmp r14, r13\n"
      "jge less_false\n"

      "inc r12\n"  ;cheack if loop again or already finished checking all args.
      "dec r15\n"
      "cmp r15,r12\n"
      "je less_true\n"
      "jmp less_loop\n"

      "less_true:\n"
      "mov rax," (find-label const-list #t)"\n"
      "jmp less_finish\n"

      "less_false:\n"
      "mov rax," (find-label const-list #f)"\n"

      "less_finish:\n"

      "leave\n"
      "ret\n"

      "less_end:\n"
      "mov [" (find-label fvar-list '<) "], rax\n\n")))

(define impl-larger
    (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, larger_code\n"
      "jmp larger_end\n"

      "larger_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r12,0\n"
      "larger_loop:\n"
      "mov r13,[rbp+8*(5+r12)]\n" ;arg
      "mov r13,[r13]\n"
      "mov r14,[rbp+8*(4+r12)]\n" ;arg
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "cmp r15,1\n"
      "je larger_true\n"

      "mov rdx,r14\n"
      "mov rcx,r13\n"
      "mov r8,1\n"
      "mov r9,1\n"
      "mov r10,r13\n"
      "mov r11,r14\n"
      "TYPE r10\n"
      "TYPE r11\n"
      "cmp r10, T_FRACTION\n"
      "je larger_splitNum2\n"

      "larger_second:\n"
      "cmp r11, T_FRACTION\n"
      "je larger_splitNum1\n"
      "jmp larger_after\n"

      "larger_splitNum2:\n"
      "DENOMINATOR rcx\n"
      "mov r8,rax\n"      ;r8 hold the DENOMINATOR of r13
      "DATA_LOWER r8\n"
      "NUMERATOR r13\n"
      "mov r13,rax\n"
      "jmp larger_second\n"

      "larger_splitNum1:\n"
      "DENOMINATOR rdx\n"
      "mov r9,rax\n"      ;r9 hold the DENOMINATOR of r14
      "DATA_LOWER r9\n"
      "NUMERATOR r14\n"
      "mov r14,rax\n"

      "larger_after:\n"

      "DATA_LOWER r14\n"
      "mov rax,r8\n"
      "mul r14\n"
      "mov r14,rax\n" ;r14=numerator(r14)*denominator(r13)

      "DATA_LOWER r13\n"
      "mov rax,r9\n"
      "mul r13\n"
      "mov r13,rax\n" ;r13=numerator(r13)*denominator(r14)

      "cmp r14, r13\n"
      "jle larger_false\n"

      "inc r12\n"  ;cheack if loop again or already finished checking all args.
      "dec r15\n"
      "cmp r15,r12\n"
      "je larger_true\n"
      "jmp larger_loop\n"

      "larger_true:\n"
      "mov rax," (find-label const-list #t)"\n"
      "jmp larger_finish\n"

      "larger_false:\n"
      "mov rax," (find-label const-list #f)"\n"

      "larger_finish:\n"

      "leave\n"
      "ret\n"

      "larger_end:\n"
      "mov [" (find-label fvar-list '>) "], rax\n\n")))

(define impl-equal
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, equal_code\n"
      "jmp equal_end\n"

      "equal_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r12,0\n"
      "equal_loop:\n"

      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "cmp r15,1\n"
      "je equal_true\n"

      "mov r13,[rbp+8*(5+r12)]\n" ;arg
      "mov r13,[r13]\n"
      "mov r14,[rbp+8*(4+r12)]\n" ;arg
      "mov r14,[r14]\n"

      "mov rdx,r14\n"
      "mov rcx,r13\n"
      "mov r8,1\n"
      "mov r9,1\n"
      "mov r10,r13\n"
      "mov r11,r14\n"
      "TYPE r10\n"
      "TYPE r11\n"
      "cmp r10, T_FRACTION\n"
      "je equal_splitNum2\n"

      "equal_second:\n"
      "cmp r11, T_FRACTION\n"
      "je equal_splitNum1\n"
      "jmp equal_after\n"

      "equal_splitNum2:\n"
      "DENOMINATOR rcx\n"
      "mov r8,rax\n"      ;r8 hold the DENOMINATOR of r13
      "DATA_LOWER r8\n"
      "NUMERATOR r13\n"
      "mov r13,rax\n"
      "jmp equal_second\n"

      "equal_splitNum1:\n"
      "DENOMINATOR rdx\n"
      "mov r9,rax\n"      ;r9 hold the DENOMINATOR of r14
      "DATA_LOWER r9\n"
      "NUMERATOR r14\n"
      "mov r14,rax\n"

      "equal_after:\n"

      "DATA_LOWER r14\n"
      "mov rax,r8\n"
      "mul r14\n"
      "mov r14,rax\n"

      "DATA_LOWER r13\n"
      "mov rax,r9\n"
      "mul r13\n"
      "mov r13,rax\n"

      "cmp r14, r13\n"
      "jne equal_false\n"

      "inc r12\n"  ;cheack if loop again or already finished checking all args.
      "dec r15\n"
      "cmp r15,r12\n"
      "je equal_true\n"
      "jmp equal_loop\n"

      "equal_true:\n"
      "mov rax," (find-label const-list #t)"\n"
      "jmp equal_finish\n"

      "equal_false:\n"
      "mov rax," (find-label const-list #f)"\n"

      "equal_finish:\n"

      "leave\n"
      "ret\n"

      "equal_end:\n"
      "mov [" (find-label fvar-list '=) "], rax\n\n")))

(define impl-is_eq
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, is_eq_code\n"
      "jmp is_eq_end\n"

      "is_eq_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"

      "mov r13,[rbp+8*5]\n" ;arg
      "mov r14,[rbp+8*4]\n" ;arg

      "cmp r14, r13\n"
      "jne is_eq_false\n"

      "is_eq_true:\n"
      "mov rax," (find-label const-list #t)"\n"
      "jmp is_eq_finish\n"

      "is_eq_false:\n"
      "mov rax," (find-label const-list #f)"\n"

      "is_eq_finish:\n"
      "leave\n"
      "ret\n"

      "is_eq_end:\n"
      "mov [" (find-label fvar-list 'eq?) "], rax\n\n")))

(define impl-numerator 
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, numerator_code\n"
      "jmp numerator_end\n"

      "numerator_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg 
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rax, r14\n"
      "mov r13, r14 \n"
      "TYPE r13\n"
      "cmp r13, T_INTEGER\n " ;if it's an integer then return the integer
      "je numerator_finish\n"

      ;it's a fraction so: 1.we minimize it.
      "DENOMINATOR rax\n"
      "mov r11,rax\n"      ;r11 hold the DENOMINATOR of r14
      "DATA_LOWER r11\n"
      "NUMERATOR r14\n"        
      "mov r13,rax\n"      ;r13 holds the NUMERATOR  of r14
      "DATA_LOWER r13\n"

      "push r11\n"
      "push r12\n"
      "push r13\n"  ;saving before doing call to gdc
      "push r14\n"
      "push r15\n"

      "shl r11,4\n"
      "add r11, T_INTEGER\n"
      "shl r13,4\n"
      "add r13, T_INTEGER\n"
      "push r13\n"
      "push r11\n"         
      "push 2\n"
      "mov rax, ["(find-label fvar-list 'gcd)"]\n"
      "mov rbx,rax\n"
      "CLOSURE_ENV rbx\n"
      "push rbx\n"
      "CLOSURE_CODE rax\n"
      "call rax\n"
      "add rsp,4*8\n"   ;rax holds gcd of the denominators a & b
      "pop r15\n"
      "pop r14\n"
      "pop r13\n"
      "pop r12\n"
      "pop r11\n"

      "shr rax,4\n"
      "mov r10,rax\n"   ;r[10] = gcd(a,b) = gcd(numerator(r14),denominator(r14))
      "mov rax,r13\n"   ;rax holds the nominator
      "cqo\n"
      "idiv r10\n"       ;rax = nominator / gcd(a,b)
      "shl rax,4\n"
      "add rax, T_INTEGER\n"

      "numerator_finish:\n"
      "mov r14,rax\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"

      "leave\n"
      "ret\n"

      "numerator_end:\n"
      "mov [" (find-label fvar-list 'numerator) "], rax\n\n")))

(define impl-denominator 
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, denominator_code\n"
      "jmp denominator_end\n"

      "denominator_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r14,[rbp+8*(4+0)]\n" ;arg 
      "mov r14,[r14]\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count
      
      "mov rax,1\n"
      "shl rax,4\n"
      "add rax, T_INTEGER\n"

      "mov r13, r14 \n"
      "TYPE r13\n"
      "cmp r13, T_INTEGER\n " ;if it's an integer then return the integer
      "je denominator_finish\n"
      "mov rax, r14\n"

      ;it's a fraction so: 1.we minimize it.
      "DENOMINATOR rax\n"
      "mov r11,rax\n"      ;r11 hold the DENOMINATOR of r14
      "DATA_LOWER r11\n"
      "NUMERATOR r14\n"        
      "mov r13,rax\n"      ;r13 holds the NUMERATOR  of r14
      "sar r13,4\n"

      "push r11\n"
      "push r12\n"
      "push r13\n"  ;saving before doing call to gdc
      "push r14\n"
      "push r15\n"

      "shl r11,4\n"
      "add r11, T_INTEGER\n"
      "shl r13,4\n"
      "add r13, T_INTEGER\n"
      "push r11\n"
      "push r13\n"         
      "push 2\n"
      "mov rax, ["(find-label fvar-list 'gcd)"]\n"
      "mov rbx,rax\n"
      "CLOSURE_ENV rbx\n"
      "push rbx\n"
      "CLOSURE_CODE rax\n"
      "call rax\n"
      "add rsp,4*8\n"   ;rax holds gcd of the denominators a & b
      "pop r15\n"
      "pop r14\n"
      "pop r13\n"
      "pop r12\n"
      "pop r11\n"

      "shr rax,4\n"
      "mov r10,rax\n"   
      "mov rax,r11\n"   ;rax holds the denominator
      "cqo\n"
      "idiv r10\n"       ;rax = denominator / gcd(a,b)
      "shl rax,4\n"
      "add rax, T_INTEGER\n"

      "denominator_finish:\n"
      "mov r14,rax\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"


      "leave\n"
      "ret\n"

      "denominator_end:\n"
      "mov [" (find-label fvar-list 'denominator) "], rax\n\n")))

(define impl-gcd  ;it's called after pushing values and not addresses.
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, gcd_code\n"
      "jmp gcd_end\n"

      "gcd_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov rdi,1\n"
      "mov r13,[rbp+8*(5+0)]\n" ;arg 1 -b
      "mov r14,[rbp+8*(4+0)]\n" ;arg 0 -a 
      "mov r15,[rbp+8*(3+0)]\n" ;arg count
      "DATA_LOWER r13\n"
      "DATA_LOWER r14\n"

      "cmp r13,0\n"
      "jl mul_with_minus\n"

      ;"cmp r14,0\n"
      ;"jl mul_with_minus\n"
      "jmp gcd_loop\n"
      "mul_with_minus:\n"
      "mov rdi,-1\n"

      "gcd_loop:\n"
      "cmp r13,0\n"
      "je gcd_finish\n"

      "xor rdx,rdx\n"
      "mov r12, r14\n"
      "mov r14,r13\n"
      "mov rax,r12\n"
      "cqo\n"
      "idiv r13\n"
      "mov r13,rdx\n"
      "jmp gcd_loop\n"

      "gcd_finish:\n"
      "mov rax,r14\n"
      "imul rdi\n"

      "cmp rax,0\n"
      "jl gcd_make_poitive\n"
      "jmp gcd_real_finish\n"
    "gcd_make_poitive:\n"
    "mov r14,-1\n"
      "mul r14\n"

      "gcd_real_finish:\n"
      "shl rax,4\n"
      "add rax, T_INTEGER\n"

      "leave\n"
      "ret\n"

      "gcd_end:\n"
      "mov rax,[rax]\n"
      "mov [" (find-label fvar-list 'gcd) "], rax\n\n")))

(define impl-plus
    (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, plus_code\n"
      "jmp plus_end\n"

      "plus_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r12,0\n"
      "mov r14,0\n"   ;r14 holds the sum till now.
      "shl r14,4\n"
      "add r14, T_INTEGER\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "plus_loop:\n"

      "cmp r12,r15\n"
      "je plus_finish\n"

      "mov r13,[rbp+8*(4+r12)]\n" ;arg
      "mov r13,[r13]\n"

      "mov rdx,r14\n"
      "mov rcx,r13\n"
      "mov r8,1\n"
      "mov r9,1\n"
      "mov r10,r13\n"
      "mov r11,r14\n"
      "TYPE r10\n"
      "TYPE r11\n"
      "cmp r10, T_FRACTION\n"
      "je plus_splitNum2\n"
      
      "plus_second:\n"
      "cmp r11, T_FRACTION\n"
      "je plus_splitNum1\n"
      "jmp plus_after\n"

      "plus_splitNum2:\n"
      "DENOMINATOR rcx\n"
      "mov r8,rax\n"      ;r8 hold the DENOMINATOR of r13
      "DATA_LOWER r8\n"
      "NUMERATOR r13\n"
      "mov r13,rax\n"
      "jmp plus_second\n"

      "plus_splitNum1:\n"
      "DENOMINATOR rdx\n"
      "mov r9,rax\n"      ;r9 hold the DENOMINATOR of r14
      "DATA_LOWER r9\n"
      "NUMERATOR r14\n"
      "mov r14,rax\n"

      "plus_after:\n"
      "sar r13,4\n"
      "sar r14,4\n"

      "push r12\n"
      "push r13\n"  ;saving before doing call to gdc
      "push r14\n"
      "push r15\n"

      "shl r8,4\n"
      "add r8, T_INTEGER\n"
      "shl r9,4\n"
      "add r9, T_INTEGER\n"
      "push r8\n"
      "push r9\n"         
      "push 2\n"
      "mov rax, ["(find-label fvar-list 'gcd)"]\n"
      "mov rbx,rax\n"
      "CLOSURE_ENV rbx\n"
      "push rbx\n"
      "CLOSURE_CODE rax\n"
      "call rax\n"
      "add rsp,4*8\n"   ;rax holds gcd of the denominators a & b
      "pop r15\n"
      "pop r14\n"
      "pop r13\n"
      "pop r12\n"

      "shr r8,4\n"
      "shr r9,4\n"
      "shr rax,4\n"

      "mov r10,rax\n"   ;r[10] = gcd(a,b)
      "mov rax, r8\n"     
      "mul r9\n"       ;rax = a*b 
      "div r10\n"       ;rax = a*b/gcd(a,b) = LCM
      "mov r11,rax\n"   ;r11 = LCM

      "cqo\n"
      "idiv r8\n"
      "mul r13\n"
      "mov r13,rax\n" ;r13 = LCM/denominator(r13) * numerator(r13) 

      "mov rax,r11\n"
      "cqo\n"
      "idiv r9\n"
      "mul r14\n"
      "mov r14,rax\n" ;r14 = LCM/denominator(r14) * numerator(r14) 

      "xor rdx,rdx\n"
      "add r13,r14\n"
      "mov rax, r13\n"
      "cqo\n"
      "idiv r11\n"
      
      "cmp rdx,0\n"
      "je plus_resIsInt\n"

      "mov r14,rdx\n"
      "mul r11\n"
      "add rax,r14\n"
      "mov r13,r11\n"   
      "mov r11,rax\n"  

      "shl r11,4\n"
      "add r11, T_INTEGER\n"
      "shl r13,4\n"
      "add r13, T_INTEGER\n"

      "mov rdi, 16\n"
      "call malloc\n"
      "test rax, rax\n"
      ; "mov rdx, rax\n\n"
      "mov qword[rax] , r11\n"
      "mov qword[rax+8] , r13\n"
      "mov r8, rax\n"
      "add rax, 8\n"
      "mov r13, rax\n"

      "MAKE_LITERAL_FRACTION2 r8 , r13\n"
      "mov r14,r8 \n"
      "inc r12\n"   
      "jmp plus_loop\n"

      "plus_resIsInt:\n"
      "mov r14,rax\n"
      "shl r14,4\n"
      "add r14, T_INTEGER\n"
      "inc r12\n"   
      "jmp plus_loop\n"

      "plus_finish:\n"

      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"

      "leave\n"
      "ret\n"

      "plus_end:\n"
      "mov [" (find-label fvar-list '+) "], rax\n\n")))

(define impl-minus  ;does not do a minimum answer alway
    (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, minus_code\n"
      "jmp minus_end\n"

      "minus_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r12,0\n"
      "mov rsi,1\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count
      "mov r14,[rbp+8*(4+r12)]\n" ;first arg
      "mov r14,[r14]\n"

      "minus_loop:\n"

      "cmp r12,r15\n" ;stop case for 1 arg only
      "je minus_finish\n"

      "cmp r15,1\n"
      "jne minus_next\n"

      "mov r13,[rbp+8*(4+r12)]\n" ;if there is only 1 arg then do 0-arg
      "mov r13,[r13]\n"
      "mov r14,T_INTEGER\n"
      "jmp minus_continue\n"

      "minus_next:\n"
      "cmp rsi,r15\n" ;stop case for when r15>1 
      "je minus_finish\n"

      "inc rsi\n"
      "mov r13,[rbp+8*(5+r12)]\n" ;r14 holds left arg and r13 hold right arg
      "mov r13,[r13]\n"

      "minus_continue:"
      "mov rdx,r14\n"
      "mov rcx,r13\n"
      "mov r8,1\n"  
      "mov r9,1\n"
      "mov r10,r13\n"
      "mov r11,r14\n"
      "TYPE r10\n"
      "TYPE r11\n"
      "cmp r10, T_FRACTION\n"
      "je minus_splitNum2\n"
      
      "minus_second:\n" ;first we check both args to see if they are a fraction and need to be split
      "cmp r11, T_FRACTION\n"
      "je minus_splitNum1\n"
      "jmp minus_after\n"

      "minus_splitNum2:\n"
      "DENOMINATOR rcx\n"
      "mov r8,rax\n"      ;r8 hold the DENOMINATOR of r13
      "DATA_LOWER r8\n"
      "NUMERATOR r13\n"
      "mov r13,rax\n"
      "jmp minus_second\n"

      "minus_splitNum1:\n"
      "DENOMINATOR rdx\n"
      "mov r9,rax\n"      ;r9 hold the DENOMINATOR of r14
      "DATA_LOWER r9\n"
      "NUMERATOR r14\n"
      "mov r14,rax\n"

      "minus_after:\n"
      "sar r13,4\n"
      "sar r14,4\n"

      "push r12\n"
      "push r13\n"  ;saving before doing call to gdc
      "push r14\n"
      "push r15\n"

      "shl r8,4\n"  ;we do gcd of the denominators
      "add r8, T_INTEGER\n"
      "shl r9,4\n"
      "add r9, T_INTEGER\n"
      "push r8\n"
      "push r9\n"         
      "push 2\n"
      "mov rax, ["(find-label fvar-list 'gcd)"]\n"
      "mov rbx,rax\n"
      "CLOSURE_ENV rbx\n"
      "push rbx\n"
      "CLOSURE_CODE rax\n"
      "call rax\n"
      "add rsp,4*8\n"   ;rax holds gcd of the denominators a & b
      "pop r15\n"
      "pop r14\n"
      "pop r13\n"
      "pop r12\n"

      "shr r8,4\n"
      "shr r9,4\n"
      "shr rax,4\n"

      "mov r10,rax\n"   ;r[10] = gcd(a,b)
      "mov rax, r8\n"     
      "mul r9\n"       ;rax = a*b 
      "cqo\n"
      "idiv r10\n"       ;rax = a*b/gcd(a,b) = LCM
      "mov r11,rax\n"   ;r11 = LCM
      "cqo\n"
      "idiv r8\n"
      "mul r13\n"
      "mov r13,rax\n" ;r13 = LCM/denominator(r13) * numerator(r13) 

      "mov rax,r11\n"
      "cqo\n"
      "idiv r9\n"
      "mul r14\n"
      "mov r14,rax\n" ;r14 = LCM/denominator(r14) * numerator(r14) 
      "xor rdx,rdx\n"
      "sub r14,r13\n" 
      "mov rax, r14\n" ;rax holds the new numerator
      "cqo\n"
      "idiv r11\n"      ;we divide it by the new denominator
      
      "cmp rdx,0\n"    
      "je minus_resIsInt\n" ;after subtraction the result is an integer

      "mov r14,rdx\n" ;the result is a fraction
      "mul r11\n"
      "add rax,r14\n"
      ;"mov r13,r11\n" ;r13 holds the denominator 
      ;"mov r11,rax\n" ;r11 holds the numerator
      "mov r9,r11\n" ;r13 holds the denominator 
      "mov r8,rax\n" ;r11 holds the numerator


      ;"mov r8,r11\n"
      ;"mov r9,r13\n"
      "push r8\n"
      "push r11\n"
      "push r12\n"
      "push r13\n"  ;saving before doing call to gdc
      "push r14\n"
      "push r15\n"

      "shl r8,4\n"  ;we do gcd of the denominators
      "add r8, T_INTEGER\n"
      "shl r9,4\n"
      "add r9, T_INTEGER\n"
      "push r8\n"
      "push r9\n"         
      "push 2\n"
      "mov rax, ["(find-label fvar-list 'gcd)"]\n"
      "mov rbx,rax\n"
      "CLOSURE_ENV rbx\n"
      "push rbx\n"
      "CLOSURE_CODE rax\n"
      "call rax\n"
      "add rsp,4*8\n"   ;rax holds gcd of the denominators a & b
      "pop r15\n"
      "pop r14\n"
      "pop r13\n"
      "pop r12\n"
      "pop r11\n"

      "shr r8,4\n"
      "shr r9,4\n"
      "shr rax,4\n"
      "pop r8\n"

      "mov r10,rax\n"

      "mov rax,r9\n"
      "cqo\n"
      "idiv r10\n"
      "mov r13,rax\n"


      "mov rax,r8\n"
      "cqo\n"
      "idiv r10\n"
      "mov r11,rax\n"

      "shl r11,4\n"
      "add r11, T_INTEGER\n"
      "shl r13,4\n"
      "add r13, T_INTEGER\n"
      "push rsi\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "test rax, rax\n"
      ; "mov rdx, rax\n\n"
      "mov qword[rax] , r11\n"
      "mov qword[rax+8] , r13\n"
      "mov r8, rax\n"
      "add rax, 8\n"
      "mov r13, rax\n"

      "MAKE_LITERAL_FRACTION2 r8 , r13\n"
      "mov r14,r8 \n"
      "inc r12\n" 
      "pop rsi\n"  
      "jmp minus_loop\n"

      "minus_resIsInt:\n"
      "mov r14,rax\n"
      "shl r14,4\n"
      "add r14, T_INTEGER\n"
      "inc r12\n"   
      "jmp minus_loop\n"

      "minus_finish:\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"
      "leave\n"
      "ret\n"

      "minus_end:\n"
      "mov [" (find-label fvar-list '-) "], rax\n\n")))

(define impl-mult
    (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, mult_code\n"
      "jmp mult_end\n"

      "mult_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r12,0\n"
      "mov r14,1\n"   ;r14 holds the sum till now.
      "shl r14,4\n"
      "add r14, T_INTEGER\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mult_loop:\n"

      "cmp r12,r15\n"
      "je mult_finish\n"

      "mov r13,[rbp+8*(4+r12)]\n" ;arg
      "mov r13,[r13]\n"

      "mov rdx,r14\n"
      "mov rcx,r13\n"
      "mov r8,1\n"
      "mov r9,1\n"
      "mov r10,r13\n"
      "mov r11,r14\n"
      "TYPE r10\n"
      "TYPE r11\n"
      "cmp r10, T_FRACTION\n"
      "je mult_splitNum2\n"
      
      "mult_second:\n"
      "cmp r11, T_FRACTION\n"
      "je mult_splitNum1\n"
      "jmp mult_after\n"

      "mult_splitNum2:\n"
      "DENOMINATOR rcx\n"
      "mov r8,rax\n"      ;r8 hold the DENOMINATOR of r13
      "DATA_LOWER r8\n"
      "NUMERATOR r13\n"
      "mov r13,rax\n"
      "jmp mult_second\n"

      "mult_splitNum1:\n"
      "DENOMINATOR rdx\n"
      "mov r9,rax\n"      ;r9 hold the DENOMINATOR of r14
      "DATA_LOWER r9\n"
      "NUMERATOR r14\n"
      "mov r14,rax\n"

      "mult_after:\n"
      "sar r13,4\n"
      "sar r14,4\n"

      "mov rax, r13\n"
      "mul r14\n"
      "mov r13,rax\n" ;r13 = numerator(r14)*numerator(r13) 

      "mov rax, r8\n"
      "mul r9\n"
      "mov r11,rax\n" ;r11 = denominator(r14)*denominator(r13)
      "cmp r11,1\n"
      "je mult_resIsInt\n"

      "push r11\n"
      "push r12\n"
      "push r13\n"  ;saving before doing call to gdc
      "push r14\n"
      "push r15\n"

      "shl r11,4\n"
      "add r11, T_INTEGER\n"
      "shl r13,4\n"
      "add r13, T_INTEGER\n"
      "push r11\n"
      "push r13\n"         
      "push 2\n"
      "mov rax, ["(find-label fvar-list 'gcd)"]\n"
      "mov rbx,rax\n"
      "CLOSURE_ENV rbx\n"
      "push rbx\n"
      "CLOSURE_CODE rax\n"
      "call rax\n"
      "add rsp,4*8\n"   ;rax holds gcd of the denominators a & b
      "pop r15\n"
      "pop r14\n"
      "pop r13\n"
      "pop r12\n"
      "pop r11\n"

      "shr rax,4\n"

      "mov r10,rax\n"   ;r[10] = gcd(a,b)
      "mov rax,r13\n"
      "cqo\n"
      "idiv r10\n"
      "mov r13,rax\n"

      "mov rax,r11\n"
      "cqo\n"
      "idiv r10\n"
      "mov r11,rax\n"

      "cmp r11,1\n"
      "je mult_resIsInt\n"
     
      "shl r13,4\n"
      "add r13, T_INTEGER\n"
      "shl r11,4\n"
      "add r11, T_INTEGER\n"

      "mov rdi, 16\n"
      "call malloc\n"
      "test rax, rax\n"
      ; "mov rdx, rax\n\n"
      "mov qword[rax] , r13\n"
      "mov qword[rax+8] , r11\n"
      "mov r8, rax\n"
      "add rax, 8\n"
      "mov r13, rax\n"

      "MAKE_LITERAL_FRACTION2 r8 , r13\n"
      "mov r14,r8 \n"
      "inc r12\n"   
      "jmp mult_loop\n"


      "mult_resIsInt:\n"
      "mov r14,r13\n"
      "shl r14,4\n"
      "add r14, T_INTEGER\n"
      "inc r12\n"   
      "jmp mult_loop\n"


      "mult_finish:\n"

      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"

      "leave\n"
      "ret\n"

      "mult_end:\n"
      "mov [" (find-label fvar-list '*) "], rax\n\n")))

(define impl-divide
    (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, divide_code\n"
      "jmp divide_end\n"

      "divide_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r12,0\n"
      "mov rsi,1\n"
      "mov r15,[rbp+8*(3+0)]\n" ;arg count
      "mov r14,[rbp+8*(4+r12)]\n"
      "mov r14,[r14]\n"

      "divide_loop:\n"

      "cmp r12,r15\n"
      "je divide_finish\n"

      "cmp r15,1\n"
      "jne divide_next\n"

      "mov r13,[rbp+8*(4+r12)]\n" ;if there is only one arg then we do 1/arg
      "mov r13,[r13]\n"

      "mov r14,1\n"
      "shl r14,4\n"
      "add r14, T_INTEGER\n"
      "jmp divide_continue\n"

      "divide_next:\n"    ;;fix the bug the we do at the end another loop if there are 2 or more args :)
      "cmp rsi,r15\n"
      "je divide_finish\n"

      "inc rsi\n"

      "mov r13,[rbp+8*(5+r12)]\n" ;arg
      "mov r13,[r13]\n"

      "divide_continue:"
      "mov rdx,r14\n"
      "mov rcx,r13\n"
      "mov r8,1\n"
      "mov r9,1\n"
      "mov r10,r13\n"
      "mov r11,r14\n"
      "TYPE r10\n"
      "TYPE r11\n"
      "cmp r10, T_FRACTION\n"
      "je divide_splitNum2\n"
      
      "divide_second:\n"
      "cmp r11, T_FRACTION\n"
      "je divide_splitNum1\n"
      "jmp divide_after\n"

      "divide_splitNum2:\n"
      "DENOMINATOR rcx\n"
      "mov r8,rax\n"      ;r8 hold the DENOMINATOR of r13
      "DATA_LOWER r8\n"
      "NUMERATOR r13\n"
      "mov r13,rax\n"
      "jmp divide_second\n"

      "divide_splitNum1:\n"
      "DENOMINATOR rdx\n"
      "mov r9,rax\n"      ;r9 hold the DENOMINATOR of r14
      "DATA_LOWER r9\n"
      "NUMERATOR r14\n"
      "mov r14,rax\n"

      "divide_after:\n"
      "sar r13,4\n"
      "sar r14,4\n"

      "mov rax, r13\n"
      "cqo\n"
      "imul r9\n"
      "mov r11,rax\n" ;r11 = denominator(r14)*numerator(r13)

      "mov rax, r8\n"
      "cqo\n"
      "imul r14\n"
      "mov r13,rax\n" ;r13 = numerator(r14)*denominator(r13) 

      "cmp r11,1\n"
      "je divide_resIsInt\n"

      "push r11\n"
      "push r12\n"
      "push r13\n"  ;saving before doing call to gdc
      "push r14\n"
      "push r15\n"

      "shl r11,4\n"
      "add r11, T_INTEGER\n"
      "shl r13,4\n"
      "add r13, T_INTEGER\n"
      "push r11\n"
      "push r13\n"         
      "push 2\n"
      "mov rax, ["(find-label fvar-list 'gcd)"]\n"
      "mov rbx,rax\n"
      "CLOSURE_ENV rbx\n"
      "push rbx\n"
      "CLOSURE_CODE rax\n"
      "call rax\n"
      "add rsp,4*8\n"   ;rax holds gcd of the denominators a & b
      "pop r15\n"
      "pop r14\n"
      "pop r13\n"
      "pop r12\n"
      "pop r11\n"

      "sar rax,4\n"

      "mov r10,rax\n"   ;r[10] = gcd(a,b)
      "mov rax,r13\n"
      "cqo\n"
      "idiv r10\n"
      "mov r13,rax\n"

      "mov rax,r11\n"
      "cqo\n"
      "idiv r10\n"
      "mov r11,rax\n"

      "cmp r11,1\n"
      "je divide_resIsInt\n"

      "mov r8,-1\n"
      "cmp r13,0\n"
      "jl divide_check_numerator\n"
      "jmp divide_check_denomentor\n"
      "divide_check_numerator:\n" ;numerator is negative
      "cmp r11,0\n"
      "jl divide_both_negative\n"
      "jmp divide_con\n"
      "divide_both_negative:\n" ;both are negative
      "mov rax,r13\n"
      "mul r8\n"
      "mov r13,rax\n"
      "mov rax,r11\n"
      "mul r8\n"
      "mov r11,rax\n" 
      "jmp divide_con\n"

      "divide_check_denomentor:\n" 
      "cmp r11,0\n"
      "jl divide_both_negative\n"


      "divide_con:\n"
      "shl r13,4\n"
      "add r13, T_INTEGER\n"
      "shl r11,4\n"
      "add r11, T_INTEGER\n"

      "push rsi\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "test rax, rax\n"
      ; "mov rdx, rax\n\n"
      "mov qword[rax] , r13\n"
      "mov qword[rax+8] , r11\n"
      "mov r8, rax\n"
      "add rax, 8\n"
      "mov r13, rax\n"

      "MAKE_LITERAL_FRACTION2 r8 , r13\n"
      "mov r14,r8 \n"
      "inc r12\n"   
      "pop rsi\n"
      "jmp divide_loop\n"


      "divide_resIsInt:\n"
      "mov r14,r13\n"
      "shl r14,4\n"
      "add r14, T_INTEGER\n"
      "inc r12\n"   
      "jmp divide_loop\n"

      "divide_finish:\n"

      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"

      "leave\n"
      "ret\n"

      "divide_end:\n"
      "mov [" (find-label fvar-list '/) "], rax\n\n")))

(define impl-cons
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, cons_code\n"
      "jmp cons_end\n"

      "cons_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r13,[rbp+8*(5+0)]\n" ;arg 1
      "mov r13,[r13]\n"

      "mov r14,[rbp+8*(4+0)]\n" ;arg 0
      "mov r14,[r14]\n"

      "mov r15,[rbp+8*(3+0)]\n" ;arg count

      "mov rdi, 16\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"
      "mov qword[rax+8] , r13\n"
      "mov r8, rax\n"
      "add rax, 8\n"
      "mov r13, rax\n"

      "MAKE_LITERAL_PAIR2 r8 , r13\n"
      "mov rax,r8\n"

      "cons_finish:\n"

      "mov r14,rax\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"

      "leave\n"
      "ret\n"

      "cons_end:\n"
      "mov [" (find-label fvar-list 'cons) "], rax\n\n")))

(define impl-integer_to_char
   (lambda ()
    (string-append

      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, integer_to_char_code\n"
      "jmp integer_to_char_end\n"

      "integer_to_char_code:\n\n"
      "push rbp\n"
      "mov rbp, rsp\n"

      "mov rax, qword [rbp + 4*8]\n"
       "mov rax,[rax]\n"

      "mov rbx, rax\n"


      "xor rax ,(T_CHAR ^ T_INTEGER)\n"

      "integer_to_char_finish:\n"

      "mov r14,rax\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"

      "leave\n"
      "ret\n"
      
      "integer_to_char_end:\n"
      "mov [" (find-label fvar-list 'integer->char) "], rax\n\n")))

(define impl-char_to_integer
   (lambda ()
    (string-append

      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, char_to_integer_code\n"
      "jmp char_to_integer_end\n"

      "char_to_integer_code:\n\n"
      "push rbp\n"
      "mov rbp, rsp\n"


      "mov rax, qword [rbp + 4*8]\n"
      "mov rax,[rax]\n"
      "mov rbx, rax\n"


      "xor rax ,(T_INTEGER ^ T_CHAR)\n"

      "char_to_integer_finish:\n"
      "mov r14,rax\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"

      "leave\n"
      "ret\n"
      
      "char_to_integer_end:\n"
      "mov [" (find-label fvar-list 'char->integer) "], rax\n\n")))

(define impl-symbol_to_string ;not working!!!!!!!!!!!!!!!
   (lambda ()
    (string-append

      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, symbol_to_string_code\n"
      "jmp symbol_to_string_end\n"

      "symbol_to_string_code:\n\n"
      "push rbp\n"
      "mov rbp, rsp\n"


      "mov rax, qword [rbp + 4*8]\n"
      "mov rax,[rax]\n"

      "SYMBOL_NAME rcx , rax\n"

      "mov rax, [rcx]\n"






      "symbol_to_string_finish:\n"
      "mov r14,rax\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r14\n"

      "leave\n"
      "ret\n"
      
      "symbol_to_string_end:\n"
      "mov [" (find-label fvar-list 'symbol->string) "], rax\n\n")))

(define impl-string_to_symbol
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc             ;address to closure\n"
      "mov rbx, 1              ;fake env pointer\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, label_code_string_to_symbol\n"
      "jmp lable_end_string_to_symbol\n\n"

      "label_code_string_to_symbol:\n"
      "push rbp\n"
      "mov rbp,rsp\n"
      "mov r15,[rbp+8*(4+0)]           ;string\n"
      "mov r14, [sym_table]         ;symi\n"
      "cmp r14, " (find-label const-list '()) "\n"
      "je create_new_sym\n\n"

      "search_in_sym_table:\n"
      "mov r14,[r14]                   ;pair\n"
      "mov r13,r14\n"
      "CAR r13                         ;[consti] (sym const)\n"
      "SYMBOL_NAME r13 , r13         ;& string\n"
      "EQUAL_STRINGS r13, r15\n"
      "cmp rax,SOB_TRUE\n"
      "je ret_this_symbol\n\n"

      "CDR_GET_POINTR r14\n"
      "cmp r14, " (find-label const-list '()) "\n"
      "je create_new_sym\n\n"

      "jmp search_in_sym_table\n\n"

      "ret_this_symbol:\n"
      "CAR_GET_POINTR r14\n"
      "mov rax, r14\n"
      "jmp finish_string_to_symbol\n\n"

      "create_new_sym:\n"
      "mov rdi,8\n"
      "call malloc\n"
      "MAKE_LITERAL_SYMBOL2 rax , r15\n"
      "mov r15, rax\n"
      "mov r13, r15\n"
      "mov r14, [sym_table]         ;symi\n"

          "MAKE_LITERAL_PAIR2 r15 ,r14\n\n"

          "mov rdi,8\n"
          "call malloc\n"
          "mov [rax], r15\n"
          "mov [sym_table],rax\n"
          "mov rax, r13\n\n"

        "finish_string_to_symbol:\n"
      "pop rbp\n"
      "ret\n\n"

      "lable_end_string_to_symbol:\n"
      "mov [" (find-label fvar-list 'string->symbol) "], rax\n\n")))

(define impl-length
  (lambda ()
    (string-append
      "mov rbp,rsp\n"
      "mov rdi, 16\n"
      "call malloc\n"
      "mov rbx, 1\n"
      "MAKE_LITERAL_CLOSURE rax, rbx, length_code\n"
      "jmp length_end\n"

      "length_code:\n\n"
      "push rbp\n"
      "mov rbp,rsp\n"

      "mov r14,[rbp+8*(4+0)]\n" ;arg 0
      "mov r14,[r14]\n"
      "mov r13,r14\n"
      "TYPE r13\n"
      "cmp r13 , T_PAIR\n"
      "jne isNilLength\n"

      "xor r15,r15\n" ; r15 is the counter
      ""
      "length_loop:\n"
      "inc r15\n"
      "CDR r14\n"
      "cmp r14 , SOB_NIL\n"
      "je length_finish\n"
      "jmp length_loop\n"


      "isNilLength:\n"
      "mov r15, 0\n"

      "length_finish:\n"

      "shl r15 , 4\n"
      "or r15, T_INTEGER\n"
      "mov rdi, 8\n"
      "call malloc\n"
      "test rax, rax\n"
      "mov qword[rax] , r15\n"

      "leave\n"
      "ret\n"

      "length_end:\n"
      "mov [" (find-label fvar-list 'meirlength) "], rax\n\n")))

(define my-list
  (lambda (val) 
    (cons val '())))

(define impl-list
  (lambda (lst1 . lsts)
    (let ((lists (cons lst1 lsts)))
      (append (map my-list lists)))))

(define impl-zero?
  (lambda(n)
    (= n 0)))

(define impl-null?
  (lambda(n)
    (null? n)))

(define impl-boolean?
  (lambda(n)
    (or (eq? n #t)(eq? n #f))))

(define my-append   ;not variadic
  (lambda(r s)
    (if (null? r) 
      s
      (cons (car r) (my-append (cdr r) s)))))

(define impl-append     ;VARIADIC
   (lambda (lst1 . lsts)

   (if (and (null? lsts)(pair? lst1)(pair? (car lst1))) (my-append (car lst1)(impl-append (cdr lst1)))
     (let ((lists (cons lst1 lsts)))

          (if (null? lst1)
            '()
            (my-append (car lists) (impl-append (cdr lists))))))))


 (define my-map     ;not variadic
   (lambda(f lst)
    (if(null? lst)
      '()
      (cons (f (car lst)) (my-map f (cdr lst))))))


(define impl-map     ;VARIADIC
   (lambda (f lst1 . lsts)
     (let ((lists (cons lst1 lsts)))
          (if (or (null? lists)(null? lst1))
            '()
        (cons (apply f (my-map car lists))
              (apply impl-map f (my-map cdr lists)))))))


(define string->file
  (lambda (target assm-str)
     (begin 
     (delete-file target)
        (let* ((target-port (open-output-file target)))
            (begin (for-each (lambda(ch) (write-char ch target-port)) (string->list assm-str)) 
        (close-output-port target-port))))))

    
(define code-gen   
  (lambda (exp major clist flist)

  (let* ((case (car exp))
         (case-seq (cadr exp)))
    (cond                 
         ((equal? case 'const) (gen-code-const exp clist flist))
         ((equal? case 'if3) (gen-code-if3 exp major clist flist))
         ((equal? case 'seq) (gen-code-seq case-seq major clist flist))
         ((equal? case 'or) (gen-code-or exp major clist flist))
         ((equal? case 'applic) (gen-code-applic exp major clist flist))
         ((equal? case 'bvar) (gen-code-bvar exp major clist flist))
         ((equal? case 'pvar) (gen-code-pvar exp major clist flist))
         ((equal? case 'fvar) (gen-code-fvar exp major clist flist))
         ((equal? case 'tc-applic) (gen-code-tc-applic exp major clist flist))
         ((equal? case 'lambda-simple) (gen-code-lambda-simple exp major clist flist))
         ; ((equal? case 'lambda-var) (gen-code-lambda-var pex major ctable ftable))
         ((equal? case 'lambda-opt) (gen-code-lambda-opt  exp major clist flist))
         ((equal? case 'define) (gen-code-define exp major clist flist))
         ((equal? case 'set) (gen-code-set exp major clist flist))
          ((equal? case 'box) (gen-code-box exp major clist flist))
          ((equal? case 'box-get) (gen-code-box-get exp major clist flist))
          ((equal? case 'box-set) (gen-code-box-set exp major clist flist))
         (else "") 
        ))))


(define lib-func
  (lambda()
    (string-append
       (impl-not)
       (impl-set-car!)
       (impl-set-cdr!)
       (impl-string-set!)
       (impl-vector-set!)
       (impl-is-zero)
       (impl-is_eq)
       (impl-is-char)
       (impl-is-pair)
       (impl-is-string)
       (impl-is-boolean)
       (impl-is-null)
       (impl-is-num)
       (impl-is-rational)
       (impl-is-proc)
       (impl-is-vector)
       (impl-is-symbol)
       (impl-is-integer)
       (impl-cons)
       (impl-remainder)
       (impl-numerator)
       (impl-denominator)
       (impl-gcd)
       (impl-smaller)
       (impl-larger)
       (impl-plus)
       (impl-minus)
       (impl-mult)
       (impl-divide)
       (impl-equal)
       (impl-integer_to_char)
       (impl-char_to_integer)
       (impl-car)
       (impl-cdr)
       (impl-string-len)
       (impl-vector-len)
       (impl-string-ref)
       (impl-vector-ref)
       (impl-make-string)
       (impl-make-vector)
       (impl-length)
       (impl-symbol_to_string)
       (impl-string_to_symbol)
      (impl-apply)
      (impl-vector)
      )))

	(define scheme-lib-func
		(lambda()
        (string-append "\n" (fold-left string-append ""
          (map 
              (lambda(e)
               (string-append (code-gen e 0 const-list fvar-list) "\n")) (pipeline (file->list "project/meir-func.scm")))))

        	
        )
    )

(define compile-scheme-file
 (lambda (source target)
    (let* ((code1 (pipeline (file->list source)))
    		(temp-table (begin (map make-const-table code1) )                )
    		(temp-label-const  (set!  const-list (label-gen (reverse (list->set (reverse const-list))) 0 "const")))
    		(temp-label-fvar   (set!  fvar-list (label-gen (reverse (list->set (reverse fvar-list))) 0 "fvar")))
        ;(temp-label-fvar-lib (set!  library-lst (label-gen (reverse (list->set (reverse library-lst))) 0 "global")))
    		(ctable const-list)
        (ftable fvar-list)
        (assm-ctable (build-const-table-assembly const-list))
        (assm-ftable (build-fvar-table-assembly fvar-list ))

   		(generated-lines1 
   			(string-append "\n" (fold-left string-append ""
   				(map 
            	(lambda(e)
               (string-append (code-gen e 0 ctable ftable) "\n" print-rax-not-void )) code1))))
                                                                  ; 
   		(my-assembly-code   (string-append assm-ctable assm-ftable (build-sym-table-assembly sym-list) "section .text\nmain:\npush rbp\n" (lib-func) (scheme-lib-func) generated-lines1  "add rsp, 1*8\n ret\n"))
   		(assembly-code (string-append (list->string(file->list "project/scheme.s"))  my-assembly-code) )
   		)
    	(string->file target assembly-code)
    	)	

    )
 )


; (compile-scheme-file "inon.scm" "inon.s" )
