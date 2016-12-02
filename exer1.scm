;to get the file load
;(load "/mnt/sdcard/src/+xer1.scm")
;(splash)


(define lst '(1 5 10 25 50))

(define lsta '(50 25 10 5 1))

(define dx 0.00001)

(define (squer x)
   (* x x))

(define (change count lst)
  (cond ((= count 0) 1)
        ((null? lst) 0)
        ((< count 0) 0)
        (else (+ (change (- count (car lst)) lst)
                 (change count (cdr lst))))))
                 
                 
(define (change-i count lst a b)
  (cond ((= count 0) a)
        ((null? lst) 0)
        ((< count 0) 0)
        (else (change-i (- count (car lst)) lst (+ a b) a))))
     
(define (f a)
  (cond ((< a 3) a)
        (else (+ (f (- a 1)) 
                 (f (- a 2))
                 (f (- a 3))))))
                 
(define (f-i a b c d)
  (cond ((< a 4) b)
        ((< a 3) a)
        (else (f-i  (- a 1) (+ b c d) b c))))
        
(define (psk a)
  (if (< a 2) '(1 1)
      (adds (psk (- a 1)) '(1))))
      
(define (psk-i a b)
  (if (< a 2) b
      (psk-i (- a 1) (adds b '(1))))) 
      
(define (adds a b)
  (if (null? (cdr a)) (append b '(1))
      (adds (cdr a) (append b (list (+ (car a) (car (cdr a))))))))
              
(define (f-s m n)
  (cond ((= n 0) 1)
        ((even? n) (squer (f-s m (/ n 2))))
        (else (* m (f-s m (- n 1))))))  
        
(define (f-s-i m n )
  (define (iter lst a)
    (cond ((null? lst) a)
          ((= (car lst) 1) (iter (cdr  lst) (squer a)))
          (else (iter (cdr lst) (* a m)))))
   (iter (reverse (asdf n '())) 1))
        
        
(define (asdf a b)
  (cond ((= a 0) b)
        ((even? a)  (asdf (/ a 2)   (append b '(1))))
        (else (asdf (- a 1) (append b '(0))))))
  
(define (f-s-ii m n a)
  (cond ((= n 1) (* m a))
        ((even?  n) (f-s-ii (squer m) (/ n 2) a))
        (else (f-s-ii m (- n 1) (* m a)))))
  
(define (fm m n)  
  (cond ((= n 0) 0)
        ((even? n) (doub (fm m (/ n 2))))
        (else (+ m (fm m (- n 1))))))
        
(define (doub x)
  (+ x x))
  
  
(define (fm-i m n a)
  (cond ((= n 1) (+ m a))
        ((even? n) (fm-i (doub m) (/ n 2) a))
        (else (fm-i m (- n 1) (+ m a)))))
  
(define (fib-iter a b p q count)
  (cond ((= count 0) b) 	
        ((even? count) 	 
           (fib-iter a b 
                (+ (* p p) (* q q)) 	
               	 (+ (* q q) (* 2 q p)) 	
               	 (/ count 2))) 
       	 (else 
           	(fib-iter 
           	(+ (* b q) (* a q) (* a p))
           	(+ (* b p) (* a q)) 			
           	 p 			 q 			 (- count 1)))))
  
(define (gcd m n)
  (if (= n 0) m
      (gcd n (remainder m n))))
    
(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))
      
      
(define (sum-it term a next b)
  (define (iter a res)
    (if (> a b) res
        (iter (next a) (+ res (term a)))))
  (iter a 0))
  
(define (product term a next b)
  (define (iter a res)
    (if (> a b) res
        (iter (next a) (* res (term a)))))
  (iter a 1))  

(define (inden x) x)

(define (pi-pr a)
  (define (term x) (/ (- (* x x) 1) (* x x)))
  (define (next x) (+ x 2))
  (* 4 (product term 3.0 next a)))
  
(define (acc combi nul term a next b)
  (define (iter a res)
    (if (> a b) res
        (iter (next a) (combi res (term a)))))
  (iter a nul))

(define (cont-frnt d n k)
  (define (f i)
    (if (= i k) (/ (d i) (n i))
      (/ (d i) (+ (n i) (f (+ i 1))))))
  (f 1))
        
(define (d x) 1.0)

(define (cont-i d n k f)
  (define (iter i res)
    (if (< i 1) res
        (iter (- i 1) (/ (d i) (f (n i) res)))))
  (iter k 0.0))

(define (n x)
  (let ((re (remainder x 3)))
    (if (or (= re 0) (= re 1)) 1
        (* 2 (/ (+ x 1) 3)))))

(define (d1 x)
  (if (= x 1) x
      (* x x)))
      
(define (n1 x) (- (* 2 x) 1))

(define (cubi a b c)
  (lambda (x) (+ (cube x) 
                 (* (squer x) a)
                 (* x b) c)))
                 
(define (double f)
  (lambda (x) (f (f  x))))        
             
(define (compose fa fb)
  (lambda (x) (fa (fb x))))

(define (repite f n)
  (define (iter x y)
    (if (< x 2) y
        (iter (- x 1) (lambda (x) (f (y x))))))
  (iter n f))
    
(define (repit f n)
  (if (< n 2) f
    (lambda (x) (f ((repit f (- n 1)) x)))))

(define (rep f n)
  (define (iter x y)
    (if (< x 2) y
      (iter (- x 1) (compose f y))))
  (iter n f))

(define (smooth f)
  (lambda (x) (/ (+ (f (+ x dx))
                    (f x)
                    (f (- x dx))) 
                 3)))








  
       
           