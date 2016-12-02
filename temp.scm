(define nil '())

(define la '(1 2 3 4))

(define lb '(6 7 8 9))

(define ma '((1 2 3 4)(5 6 7 8)(9 10 11 12)(13 14 15 16)))

(define mb '((1 2 4 6)(2 3 8 7)(3 4 3 9)(4 5 7 4)))

(define (reversea a)
  (if (null? (cdr a)) a
      (append (reversea (cdr a)) (list(car a)))))
      
(define (last a)
  (if (null? (cdr a)) (car a)
      (last (cdr a))))

(define (appends a b)
  (if (null? a) b
     (cons (car a) (appends (cdr a) b))))

(define (rever a)
  (define (iter x b)
    (if (null? x) b
        (iter (cdr x) (cons (car x) b))))
   (iter a '()))

(define (same x . y)
  (define (iter a)
  (let ((z (remainder x 2)))
    (cond ((null? a) a)
          ((= z (remainder (car a) 2))
           (cons (car a) (iter (cdr a))))
          (else (iter (cdr a))))))
  (iter y))
    
(define (maps proc lst)
  (if (null? lst) lst
      (cons (proc (car lst)) 
            (maps proc (cdr lst)))))

(define (mapc proc lst)
  (define (iter a b)
    (if (null? a) b
        (iter (cdr a) (appends b (list(proc (car a)))))))
  (iter lst '()))

(define (foreach proc lst)
  (define (iter f a b)
    (if (not(null? a))
        (iter f (cdr a) (f (car a)))))
  (iter proc lst '()))

(define (pro . y)
  (define (iter a b)
    (if (not (null? a))
        (iter (cdr a) (eval (car a)))))
  (iter y nil))

(define (count-ls x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-ls (car x))
                 (count-ls (cdr x))))))

(define (fringe x)
  (cond ((null? x) x)
        ((not (pair? x)) (cons x nil))
        (else (append (fringe (car x)) (fringe (cdr x))))))
 
       
(define (map-tree proc x)
  (cond ((null? x) x)
        ((not (pair? x)) (proc x))
        (else (cons (map-tree proc (car x)) (map-tree proc (cdr x))))))


(define (subset a)
  (if (null? a) (list a)
     (let ((rest (subset (cdr a))))
       (append rest (map (lambda (x) (cons  (car a) x)) rest)))))


(define (acc op ini sq)
  (if (null? sq) ini
      (op (car sq) (acc op ini (cdr sq)))))

(define (map-acc op lst)
  (acc (lambda (x y) (cons (op x) y)) nil lst))

(define (append-acc x y)
  (acc cons y x))

(define (length-acc lst)
  (acc (lambda(x y)(+ y 1)) 0 lst))

(define (horner lst x)
  (acc (lambda(a b) (+ a (* x b))) 0 lst))

(define (count-ls-acc lst)
  (acc (lambda(x y)(+ (length x) y))
       0 
       (map (lambda(x)(if (not(pair? x)) (list x )(fringe x))) lst)))

(define (acc-n op ini s)
  (if (null? (car s)) nil
      (cons (acc op ini (map car s))
            (acc-n op ini (map cdr s)))))
            
(define (turn m)
  (if (null? (car m)) nil
      (cons (map car m) (turn (map cdr m)))))

(define (turn-n m)
  (acc-n cons nil m))
 
(define (dot-pro m n)
  (acc + 0 (map * m n)))

(define (mxv m v)
  (map (lambda(x) (dot-pro x v)) m))

(define (mxmn m n)
  (map (lambda(x) (mxv(turn n) x)) m))

(define (mxmm m n)
  (map (lambda(x) (mxv m x))(turn n)))

(define (acc-left op ini lst)
  (define (iter a b)
    (if (null? a) b
        (iter (cdr a) (op b (car a)))))
  (iter lst ini))

(define (rev-r s)
  (acc (lambda(x y) (append y (list x))) nil s))

(define (rev-l s)
  (acc-left (lambda(x y) (cons y x)) nil s))

(define (enum x y)
  (if (> x y) nil
    (cons x (enum (+ x 1) y))))

(define (flatmap proc sq)
  (acc append nil (map proc sq)))

(define (test x)
  (flatmap (lambda (i) (list (map (lambda (j) (list i j)) (enum 1 x))))
           (enum 1 x)))
      
(define (queen x)
  (if (null? (cdr x)) (map list (car x))
  (filter (lambda (x) (exam (car x) (cdr x))) (flatmap (lambda(i) (map (lambda (j) (cons j i)) (car x)))
           (queen (cdr x))))))
 
(define (exam j i)
  (cond ((null? i) #t)
        ((no j (car i)) #f)
        (else (exam j (cdr i)))))
  
(define (no x y)
  (or (= (cadr x) (cadr y))
      (= (- (cadr x) (car x))
         (- (cadr y) (car y)))
      (= (+ (cadr x) (car x))
         (+ (cadr y) (car y)))
       ))

(define (filter p s)
  (cond ((null? s) s)
        ((p (car s))
         (cons (car s) (filter p (cdr s))))
        (else (filter p (cdr s)))))
        

(define (queens x)
  (if (null? (cdr x)) (map list (car x))
   (flatmap (lambda(i) (map (lambda (j) (cons j i)) (car x)))
           (queens (cdr x)))))





















