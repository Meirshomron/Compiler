(load "project/pc.scm")

(define <sexpr>
		(new 
			(*delayed (lambda() <InfixExpression>))
			(*delayed (lambda() <Number>))
			(*delayed (lambda() <ProperList>))
			(*delayed (lambda() <ImproperList>))
			(*delayed (lambda() <Vector>))
			(*delayed (lambda() <Quoted>))
			(*delayed (lambda() <QuasiQuoted>))
			(*delayed (lambda() <UnquotedAndSpliced>))
			(*delayed (lambda() <Unquoted>))
			(*delayed (lambda() <Symbol>))
			(*delayed (lambda() <Boolean>))
			(*delayed (lambda() <String>))							
			(*delayed (lambda() <CBName>))
			(*delayed (lambda() <Char>))
			
			(*disj 14)
			done )
		)
  ;; fill in the s-expression parser details here
(define <newline_or_eof>
	(new (*parser (char #\newline))
		 (*parser <end-of-input>)
		 (*disj 2)
		 done)) 

(define <whitespace> (const (lambda(ch ) (char<=? ch #\space))) )

(define <Line_comments>
	(new (*parser <whitespace>) *star
	     (*parser (char #\;))
		 (*parser <any-char>) (*parser <newline_or_eof>)*diff *star
		 (*parser <newline_or_eof>)
		 (*parser <whitespace>) *star		 
		 (*caten 5)
		 done))

(define <Exp_comments>
	(new (*parser <whitespace>) *star
		 (*parser (char #\#))
		 (*parser (char #\;))
		 (*parser <whitespace>) *star
		 (*delayed (lambda() <InfixAddSub>))(*parser <sexpr>)(*disj 2)

		 (*parser <whitespace>) *star
		 (*caten 6) 
		 done))

(define <comments>
	(new (*parser <Exp_comments>)
		 (*parser <Line_comments>)
		 (*disj 2)
		 done))

(define <comments_or_space>
	(new 
		(*parser <comments>) *plus
		(*parser <whitespace>) *star
			 
		 (*disj 2)
		 done))

(define <leftBracket> 
	(new	(*parser (word-ci "(" ))
		(*pack (lambda (x) "("))done))

(define <rightBracket> 
	(new	(*parser (word-ci ")" ))
		(*pack (lambda (x) ")"))done))


(define <digit-0-9> (range #\0 #\9))
(define <letter-a-z> (range #\a #\z))
(define <letter-A-Z> (range #\A #\Z))



;boolean is a "#f" | "#t" followed by a whitespace or end-of-line.
(define <Boolean> 
	(new (*parser <comments_or_space>)
		 (*parser (word-ci "#f"))
		 (*caten 2)
	     (*pack-with (lambda(sp _)#f))

	     (*parser <comments_or_space>)
	     (*parser (word-ci "#t"))
	     (*caten 2)
	     (*pack-with (lambda(sp _)#t))

	     (*disj 2)
	     (*parser <comments_or_space>)(*parser <end-of-input>)(*caten 2)(*parser <epsilon>)(*disj 2)
	     (*caten 2)
	     (*pack-with (lambda(bool sp)bool))

	     (*parser <letter-a-z>)
	     *not-followed-by
	     (*parser <letter-A-Z>)
	     *not-followed-by
	     (*parser <digit-0-9>)
	     *not-followed-by

	     done
	))

;the char #\ 
(define <CharPrefix> 
	(new (*parser (char #\#))
		 (*parser (char #\\))
		 (*caten 2)
		 (*pack-with (lambda(a b) "#\\"))
		done))

;all the characters larger then the space char in ascii table
(define <VisibleSimpleChar> (const (lambda(ch ) (char>? ch #\space))))

;the char x
(define <x> (const (lambda(ch ) (or (char=? ch #\x)(char=? ch #\X)))))

;saving the special words as a type of char
(define <NamedChar> 
	(new (*parser (word-ci "lambda"))
	     (*pack (lambda(_) (integer->char 955)))
	     (*parser (word-ci "newline"))
	     (*pack (lambda(_) #\newline))
	     (*parser (word-ci "nul"))
	     (*pack (lambda(_)(integer->char 0)))
	     (*parser (word-ci "page"))
	     (*pack (lambda(_)#\page))
	     (*parser (word-ci "return"))
	     (*pack (lambda(_) #\return))
	     (*parser (word-ci "space"))
	     (*pack (lambda(_) #\space))
	     (*parser (word-ci "tab"))
	     (*pack (lambda(_) #\tab))

	     (*disj 7)

	     done))

;all the chars 0 till 9 and a till f
(define <HexChar> 
	(new (*parser <digit-0-9>)
		 (*parser (range-ci #\a #\f))
		 (*disj 2)done
		))

;all of that start with x and contine as a hex number. for example: x12341fa
(define <HexUnicodeChar> 
	(new (*parser <x>)
		 (*parser <HexChar>)*plus
		 (*parser <whitespace>) *star
		  (*caten 3)
		  (*pack-with  (lambda (x l s) (integer->char(string->number (list->string (append (list #\#)  (list #\x) l))))))
		  done))


;a disjoint group of all char types.
(define <Char> 
	(new (*parser <comments_or_space>)
		 (*parser <CharPrefix>)
		 (*parser <comments_or_space>) 
		 (*caten 3)(*pack-with (lambda(cs1 ch cs2)ch))

		 (*parser <NamedChar>)
		 (*parser <HexUnicodeChar>)
		 (*parser <VisibleSimpleChar>)
		 (*disj 3)
		 (*parser <comments_or_space>) 
		 (*caten 2)
		 (*pack-with (lambda(ch cs)ch))

		 (*caten 2)
		 (*pack-with (lambda(pr ch)ch))

		 done))

(define <Natural> 
	(new (*parser <digit-0-9>) *plus 
		(*pack (lambda (x)(string->number (list->string `(,@x)) )))
		done))

(define <Integer> 
	(new (*parser (char #\+))
		(*parser <Natural>)
		(*caten 2)
		(*pack-with (lambda (_ n) n))

		(*parser (char #\-))
		(*parser <Natural>)
		(*caten 2)
		(*pack-with (lambda (_ n) (- n)))

		(*parser <Natural>)
		(*disj 3)
		done		

		))

(define <Fraction> 
	(new 
		(*parser <Integer>)
		(*parser (char #\/))
		(*parser <Natural>)
		(*caten 3)
		(*pack-with (lambda (numerator _ denominator) (/ numerator denominator )))
		done		

		))

(define <Number>
	(new
	    (*parser <comments_or_space>)(*parser <whitespace>) *star (*disj 2)
	   (*parser <Fraction>)
	   (*parser <Integer>)
	   
	   (*disj 2)
	   (*parser <comments_or_space>)(*parser <end-of-input>)(*caten 2)(*parser <epsilon>)(*parser <comments_or_space>)(*disj 3)
	   (*caten 3)
	   (*pack-with (lambda(cs1 num cs2) num))

	     (*delayed(lambda() <SymbolChar>))
	     *not-followed-by

	   done
	))

(define <StringLiteralChar> 
	(const (lambda(ch ) (and (not(char=? ch #\\)) (not(char=? ch #\")) )))
		)

(define <StringMetaChar> 
	(new
		(*parser (char #\\))
		(*parser (char #\n))
		(*caten 2)
		(*pack-with (lambda(i x) #\newline))
		(*parser (char #\\))
		(*parser (char #\\))
		(*caten 2)
		(*pack-with (lambda(i x) #\\))
		(*parser (char #\\))
		(*parser (char #\"))
		(*caten 2)
		(*pack-with (lambda(i x) #\"))
		(*parser (char #\\))
		(*parser (char #\t))
		(*caten 2)
		(*pack-with (lambda(i x) #\tab))
		(*parser (char #\\))
		(*parser (char #\f))
		(*caten 2)
		(*pack-with (lambda(i x) #\page))
		(*parser (char #\\))
		(*parser (char #\r))
		(*caten 2)
		(*pack-with (lambda(i x) #\return))
		(*disj 6)
		
		done))

(define <StringHexChar> 
	(new
		(*parser (char #\\))
		(*parser (char #\x))
		(*parser <HexChar>) *star
		(*parser (char #\;))
		(*caten 4)
		(*pack-with  (lambda (s x  l e) (integer->char(string->number (list->string (append (list #\#)  (list #\x) l))))))

		done))

(define <StringChar>
	(new 
		(*parser <StringMetaChar>)
		(*parser <StringLiteralChar>)	 
		(*parser <StringHexChar>)
		(*disj 3)
		done))

(define <String> 
	(new (*parser <comments_or_space>)
		 (*parser (char #\"))
		 (*parser <StringChar>) *star
		 (*parser (char #\"))
		 (*parser <comments_or_space>)
		 (*caten 5)
		 (*pack-with (lambda (sp1 left lst right sp2) (list->string lst)))
		done))

(define <SymbolChar>
	(new (*parser <digit-0-9>)
		(*parser <letter-a-z>)
		(*parser <letter-A-Z>)
		(*pack (lambda(ch) (integer->char (+ (char->integer ch ) 32))))
		(*parser (char #\!))
		(*parser (char #\$))
		(*parser (char #\^))
		(*parser (char #\*))
		(*parser (char #\-))
		(*parser (char #\_))
		(*parser (char #\=))
		(*parser (char #\+))
		(*parser (char #\<))
		(*parser (char #\>))
		(*parser (char #\?))
		(*parser (char #\/))
		(*disj 15)
		done
		))


(define <Symbol>
	(new (*parser <comments_or_space>)
		(*parser <SymbolChar>) *plus
		(*parser <comments_or_space>)
		(*caten 3)
		(*pack-with (lambda (sp1 x sp2) (string->symbol(list->string x) )))
		done))

(define <ProperList>
	(new
		(*parser <comments_or_space>)
		(*parser <leftBracket>)
		(*parser <comments_or_space>) (*parser <sexpr>)(*parser <comments_or_space>) (*caten 3) 
		(*pack-with (lambda(space1 sexp space2)#| (display "ProperList: ")(display sexp)  |#sexp)) *star
		(*parser <sexpr>)(*parser <comments_or_space>) (*caten 2)(*pack-with (lambda (exp space) exp))
		(*parser <epsilon>) (*disj 2)
		(*caten 2)
		(*pack-with (lambda (left right) (if (null? right)(append left right) (append left (list right)))))
		(*parser <rightBracket>)(*parser <end-of-input>)(*disj 2)
		(*parser <comments_or_space>)

		(*caten 5)
		(*pack-with (lambda (sp1 lb sexp rb sp2) sexp))

		done))

(define <ImproperList>
	(new
		(*parser <comments_or_space>)
		(*parser <leftBracket>)

		(*parser <comments_or_space>)(*parser <sexpr>) *plus (*parser <comments_or_space>) (*caten 3) 
		 (*pack-with (lambda(space1 sexp space2)  sexp)) 
		 *plus
		(*parser (char #\.))
		(*parser <comments_or_space>)

		(*parser <comments_or_space>)(*parser <sexpr>)(*parser <comments_or_space>)(*caten 3) 
		(*pack-with (lambda(space1 sexp space2) sexp)) 


		(*parser <rightBracket>)
		(*parser <comments_or_space>)
		(*caten 8)
		(*pack-with (lambda (sp1 lb sexp1 dot space sexp2 rb sp2) (append (car sexp1) sexp2)))
		done))

(define <Vector>
	(new 
		(*parser <comments_or_space>)
		(*parser (char #\#))
		(*parser <ProperList>)
		(*caten 3)
		(*pack-with (lambda (sp hash l) `#(,@l)))
		done))

(define <Quoted>
	(new 
		(*parser <comments_or_space>)
		(*parser (char #\'))
		(*pack (lambda (_) "'"))
		(*parser <sexpr>)
		(*parser <comments_or_space>)
		(*caten 4)
		(*pack-with (lambda (sp1 x str sp2) (list 'quote str)))
		done))

(define <QuasiQuoted>
	(new 
		(*parser <comments_or_space>)
		(*parser (char #\`))
		(*parser <sexpr>)
		(*parser <comments_or_space>)

		(*caten 4)
		(*pack-with (lambda (sp1 _ str sp2) (list `quasiquote str)))
		done))

(define <Unquoted>
	(new
		(*parser <comments_or_space>)
		(*parser (char #\,))
		(*parser <sexpr>)
		(*parser <comments_or_space>)

		(*caten 4)
		(*pack-with (lambda (sp1 _ str sp2) (list `unquote str)))
		done))

(define <UnquotedAndSpliced>
	(new 
		(*parser <comments_or_space>)
		(*parser (char #\,))
		(*parser (char #\@))
		(*parser <sexpr>)
		(*parser <comments_or_space>)
		(*caten 5)
		(*pack-with (lambda (sp1 f s str sp2) (list `unquote-splicing str)))
		done))

(define <CBNameSyntax1>
	(new 
		(*parser (char #\@))
		(*parser <comments_or_space>)
		(*parser <sexpr>)
		(*parser <comments_or_space>)
		(*caten 4)
		(*pack-with (lambda (sh sp1 x sp2) (list 'cbname x )))
		done))

(define <CBNameSyntax2>
	(new 
		(*parser (word-ci "{"))
		(*parser <comments_or_space>)
		(*parser <sexpr>)
		(*parser <comments_or_space>)
		(*parser (word-ci "}"))
		(*caten 5)
		(*pack-with (lambda(lb sp1 x sp2 rb)(list 'cbname x )))
		done))

(define <CBName>
	(new
		(*parser <comments_or_space>)
		(*parser <CBNameSyntax1>)
		(*parser <CBNameSyntax2>)
		(*disj 2)
		(*parser <comments_or_space>)
		(*caten 3)
		(*pack-with (lambda(sp1 x sp2) x ))
		done))
;--------------------------------INFIX-----------------------------------------------------


(define reverse
	(lambda (lst)
  		(if (null? lst)
    	 '()
     (append (reverse (cdr lst)) (list (car lst)))
  	)
))


(define order-pow 
 	(lambda (lst base)

 		(cond ((null? lst)
 					lst)
 			((=(length lst)1)
 				(cdr (car lst)))
    	 (else 
    	 (let ((head (car lst))
    	 		(tail (cdr lst)))
    	 	(let ((op (car head))
    	 		 (exp (cdr head)))

    	 		 (list (append (list op)  exp (order-pow tail base)))

    	 		 )
     )))
))

(define order 
 	(lambda (lst base)
 		(if (null? lst)
 			(if (null? base) 
 				base 
 				(list base))
    	 
    	 (let ((head (car lst))
    	 		(tail (cdr lst)))
    	 	(let ((op (car head))
    	 		 (exp (cdr head)))
    	 		 
    	 		 (list (append (list op) (order tail base) exp)))
     )))
)

(define modify-neg 
 	(lambda (lst)
 		(if (null? lst)
 			lst
 			
    	 (let ((head (car lst))
    	 		(tail (cdr lst)))
    	     	

    	 	(if (pair? head)
    	 		(let ((op (car head))
    	 		 (exp (cdr head)))

 					(if (eq? op '-)
 					(list op (append exp (let ((rec-res (modify-neg tail))) (if (null? rec-res ) rec-res (list rec-res))  )) ) 
 					 (append head tail )
 					)
    	 		 )
    	 		(if (null? tail) head (append (list head) tail ))
    	 		)    	 	
     )))
)

(define <NotinfixSymbol>
	(new 
		(*parser (char #\+))
		(*parser (char #\-))
		(*parser (char #\*))
		(*parser (char #\*))(*parser (char #\*))(*caten 2)
		(*parser (char #\^))
		(*parser (char #\/))
		(*disj 6)
		done))

(define <InfixSymbolCharPlus>
	(new 
		(*parser <SymbolChar>)(*parser <NotinfixSymbol>) *diff *plus		
		(*pack (lambda (x) (string->symbol(list->string x) )))
		
		done))

(define <InfixSymbol> 

	(new 
		(*parser <comments_or_space>)
		(*parser <InfixSymbolCharPlus>)
		(*parser <comments_or_space>)
		(*caten 3)
		(*pack-with (lambda(sp1 sym sp2) sym)) 

		done)) 

(define <InfixPrefixExtentionPrefix>
	(new 
		(*parser (char #\#))
		(*parser (char #\#))
		(*caten 2)
		(*parser (char #\#))
		(*parser (char #\%))
		(*caten 2)
		(*disj 2)
		(*pack-with (lambda (left right) (append (list left) (list right))))
	done))

(define <InfixNumber>
	(new
	    (*parser <comments_or_space>)(*parser <whitespace>) *star (*disj 2)
	   (*parser <Fraction>)
	   (*parser <Integer>)
	   
	   (*disj 2)
	   (*parser <epsilon>)(*parser <comments_or_space>)(*disj 2)
	   (*caten 3)
	   (*pack-with (lambda(cs1 num cs2) num))
	   (*parser <letter-a-z>)
	   *not-followed-by
	   	   (*parser <letter-A-Z>)
	   *not-followed-by

	   done))

(define <InfixNeg>
	(new 
		(*parser (char #\-))
		(*parser <whitespace>) *plus (*delayed (lambda () <InfixNumber>))(*caten 2) (*pack-with (lambda(sp x)  (list '- x)))
		(*delayed (lambda () <InfixNumber>))(*pack (lambda(x) (* -1 x)))(*disj 2)
		(*parser <whitespace>) *plus (*delayed (lambda () <InfixSymbolCharPlus>))(*caten 2) (*pack-with (lambda(sp x)  (list '- x)))
		(*delayed (lambda () <InfixSymbolCharPlus>))(*pack (lambda(x) (list '- x)))(*disj 2)
		(*delayed (lambda () <InfixAddSub>)) (*pack (lambda(x) (list '- x)))
		(*disj 3)
		(*caten 2)
		(*pack-with (lambda (op right) right ))
		done))

(define <Atoms>
	(new (*parser <comments_or_space>)(*parser <leftBracket>)(*parser <comments_or_space>)
		 (*delayed (lambda ()<InfixAddSub>))(*parser <comments_or_space>)(*parser <rightBracket>)(*parser <comments_or_space>)(*caten 7)(*pack-with (lambda (sp1 bl sp2 lst sp3 br sp4) lst))
		(*parser <InfixNeg>)
		(*parser <InfixNumber>)
		(*parser <InfixSymbol>)

		(*disj 4)

	done))

(define <InfixArgList>
	(new 
		(*parser <comments_or_space>)(*delayed (lambda ()<InfixAddSub>))(*caten 2)(*pack-with (lambda(sp1 x) x))
		(*parser <comments_or_space>)(*parser (word ","))
		(*parser <comments_or_space>)(*delayed (lambda ()<InfixAddSub>))(*parser <comments_or_space>)(*caten 5) (*pack-with (lambda (sp1 comma sp2 x sp3) x)) *star
		(*caten 2)
		(*pack-with (lambda (first second)(append (list first) second)))

		(*parser <epsilon>)
		(*disj 2)
	done))

(define <InfixFuncall>
	(new 
		(*parser <comments_or_space>)
		(*parser <leftBracket>) (*pack (lambda(x) x))
		(*parser <comments_or_space>)
		(*parser <InfixArgList>)(*pack (lambda(x) x))
		(*parser <comments_or_space>)
		(*parser <rightBracket>)(*parser <end-of-input>)(*disj 2)
		(*parser <comments_or_space>)
		(*caten 7)
		(*pack-with (lambda (sp1 bl sp2 lst sp3 br sp4) lst))
	done))

(define <InfixArrayGet>
	(new 
		
		(*parser (word "["))
		(*delayed (lambda ()<InfixAddSub>))
		(*parser (word "]"))
		(*caten 3)
		(*pack-with (lambda (bl sexp br) sexp))
	done))

(define <GetAndCall>
	(new
		(*parser <InfixArrayGet>)
		(*parser <InfixFuncall>)
		(*disj 2)

	done))
(define func
 (lambda (lst base) 
 	(if(and (list? lst)(= (length lst) 2))
 	 (list (car lst) base (cadr lst))
 	  (list (car lst) (func (cadr lst) base) (caddr lst) ))))

(define <InfixFuncallArrayGet>
(new 
		(*parser <comments_or_space>)
		(*parser <Atoms>) 
		(*parser <comments_or_space>)
		(*parser <InfixArrayGet>) *plus 
		(*parser <comments_or_space>)

		(*caten 5)
		(*pack-with (lambda(sp1 atm sp2 ag-lst sp3) 
		(if (and (pair? atm) (eq? (car atm ) '-)) (list '- (func (car (order (reverse (map (lambda (acc)  (list 'vector-ref  acc)) ag-lst )) '())) (cadr atm) 
			))
			(func (car (order (reverse (map (lambda (acc)  (list 'vector-ref  acc)) ag-lst )) '())) atm 
			) )))
		(*parser <comments_or_space>)
		(*parser <InfixFuncall>) *plus (*parser <epsilon>)(*disj 2)
		(*parser <comments_or_space>)
		(*caten 4)

		(*pack-with (lambda(atm sp3 addOn sp4) 
		(if (and (pair? atm) (eq? (car atm ) '-))  
			(modify-neg (car (fold-left (lambda (acc x)  (list (append acc x) ))        (list atm) addOn ))	)
			(car (fold-left (lambda (acc x)  (list (append acc x) ))        (list atm) addOn )))))

		(*parser <comments_or_space>)
		(*parser <Atoms>) 
		(*parser <comments_or_space>)
		(*parser <InfixFuncall>) *plus 
		(*parser <comments_or_space>)

		(*caten 5)
		(*pack-with (lambda(sp1 atm sp2 ag-lst sp3) 
		(if (and (pair? atm) (eq? (car atm ) '-))  
			(modify-neg (car (fold-left (lambda (acc x)  (list (append acc x) ))        (list atm) ag-lst ))	)
			(car (fold-left (lambda (acc x)  (list (append acc x) ))        (list atm) ag-lst )))))
		
	 	(*parser <comments_or_space>)
	 	(*parser <InfixArrayGet>) *plus (*parser <epsilon>)(*disj 2)
		(*parser <comments_or_space>)
		(*caten 4)
		
 		(*pack-with (lambda(atm sp2 addOn sp3)
		(if (not(eq? addOn '()))(if (and (pair? atm) (eq? (car atm ) '-)) (list '- (func (car (order (reverse (map (lambda (acc)  (list 'vector-ref  acc)) addOn )) '())) (cadr atm) 
			))
			(func (car (order (reverse (map (lambda (acc)  (list 'vector-ref  acc)) addOn )) '())) atm 
			) )
		atm
		)) )
 		
		(*parser <comments_or_space>)
		(*parser <InfixPrefixExtentionPrefix>)
		(*parser <comments_or_space>)
		(*parser <sexpr>)
		(*parser <comments_or_space>)
		(*caten 5)
		(*pack-with (lambda(sp1 pf sp2 sexp sp3) sexp)) 

		(*parser <comments_or_space>)
		(*parser <Atoms>)
		(*parser <comments_or_space>)
		(*caten 3)
		(*pack-with (lambda(sp1 at sp2) at)) 

		(*disj 4)
		done))

(define <InfixPow>
	(new 
		(*parser <comments_or_space>)
		
		(*parser <InfixFuncallArrayGet>)

		(*parser <comments_or_space>)
		(*parser (char #\*))(*parser (char #\*))(*caten 2)(*parser (char #\^))(*disj 2)
		(*parser <comments_or_space>)
		(*parser <InfixFuncallArrayGet>)
		(*parser <comments_or_space>)
		(*caten 5)
		(*pack-with (lambda (sp0 op sp1 right sp2)(list 'expt right)))

		*star
		(*parser <comments_or_space>)

		(*caten 4)
		(*pack-with (lambda (sp1 left right sp3)
			(if (eq? right '()) left (append (list 'expt left) (order-pow right left) ))))
		
		done))

(define <InfixMulDiv>
	(new 
		(*parser <comments_or_space>)
		
		(*parser <InfixPow>)

		(*parser <comments_or_space>)
		(*parser (char #\*))(*parser (char #\/))(*disj 2)
		(*parser <comments_or_space>)
		(*parser <InfixPow>)
		(*parser <comments_or_space>)
		(*caten 5)
		(*pack-with (lambda (sp0 op sp1 right sp2) (if (eq? op #\*) (list '* right) (list '/ right))))

		*star
		(*parser <comments_or_space>)

		(*caten 4)
		(*pack-with (lambda (sp1 left right sp2)
	     (let ((ans (if (eq? right '()) (if (and (pair? left)(eq? (car left) '())) (cadr left) left)  (car (order (reverse (if (and (pair? right)(eq? (car right) '())) (cdr right) right)) (if (and (pair? left)(eq? (car left) '())) (cadr left) left) )))))
	    	 ans)))
		
		done))

(define <InfixAddSub>
	(new 
		(*parser <comments_or_space>)

		(*parser <InfixMulDiv>)

		(*parser <comments_or_space>)
		(*parser (char #\+))(*parser (char #\-))(*disj 2)
		(*parser <comments_or_space>)
		(*parser <InfixMulDiv>)
		(*parser <comments_or_space>)
		(*caten 5)
		(*pack-with (lambda (sp0 op sp1 right sp2) (if (eq? op #\+) (list '+ right) (list '- right))))

		*star
		(*parser <comments_or_space>)

		(*caten 4)
		(*pack-with (lambda (sp1 left right sp2)
	     (let ((ans (if (eq? right '()) (if (and (pair? left)(eq? (car left) '())) (cadr left) left)  (car (order (reverse (if (and (pair? right)(eq? (car right) '())) (cdr right) right)) (if (and (pair? left)(eq? (car left) '())) (cadr left) left) )))))
	    	 ans)))

		done))

(define <InfixExpression>
	(new 
		(*parser <comments_or_space>)
		(*parser <InfixPrefixExtentionPrefix>)
		(*parser <InfixAddSub>)
		(*caten 3)
		(*pack-with (lambda(sp1 pf a) a))

		done))

