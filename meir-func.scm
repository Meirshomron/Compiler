; (define my-reverse
;   (lambda (lst)
;       (if (null? lst)
;       '()
;        (append (reverse (cdr lst)) (list (car lst))))
;   )
; ) 
; (define plus
;   (lambda(x) (+ x x)))


(define my-append   ;VARIADIC????
  (lambda(r s)
    (if (null? r) 
      s
      (cons (car r) (my-append (cdr r) s)))))

(define impl-append     ;VARIADIC
   (lambda (lst1  lsts)
      (if (null? lsts) lst1
         (my-append lst1 (impl-append (car lsts) (cdr lsts))))))

(define append     ;VARIADIC
   (lambda l
       (if (null? l) 
          '()

           (impl-append (car l) (cdr l)))))

;-------------------------------------------
;-------------------------------------------

 ; (define my-map     ;not variadic
 ;   (lambda(f lst)
 ;    (if(null? lst)
 ;      '()
 ;      (cons (f (car lst)) (my-map f (cdr lst))))))


; (define map     ;VARIADIC
;    (lambda (f lst1 . lsts)
;      (let ((lists (cons lst1 lsts)))
;           (if (or (null? lists)(null? lst1))
;             '()
;         (cons (apply f (my-map car lists))
;               (apply map f (my-map cdr lists)))))))


(define map
                    ((lambda(y) 
                        ((lambda(map1)
                            ((lambda(maplist)
                                (lambda(f . s)
                                    (maplist f s)))
                                        (y (lambda(maplist)
                                                (lambda(f s)
                                                    (if (null? (car s)) '()
                                                        (cons (apply f (map1 car s))
                                                            (maplist f (map1 cdr s)))))))))
                            (y (lambda(map1)
                                (lambda(f s)
                                    (if (null? s) '()
                                        (cons (f (car s))
                                            (map1 f (cdr s)))))))))
                        (lambda (f)
                            ((lambda(x)
                                (f (lambda(y z)
                                        ((x x) y z))))
                            (lambda (x)
                                (f (lambda(y z)
                                        ((x x) y z))))))))

; (define map (lambda(function list1 . more-lists)
;   (letrec ((some? (lambda (function list)
;                         (and (pair? list)
;                         (or (function (car list))
;                         (some? function (cdr list))))))
;            (map1 (lambda(function list)
;                        (if (null? list)
;                            '()
;                            (cons (function (car list)) (map1 function (cdr list)))))))

;      (let ((lists (cons list1 more-lists)))
;            (if (some? null? lists)
;                '()
;                (cons (apply function (map1 car lists)) (apply map function (map1 cdr lists))))))))











;-------------------------------------------
;-------------------------------------------
(define list
    (lambda x x))