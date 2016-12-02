(define (gcd x y)
  (if (= y 0) x
      (gcd y (remainder x y))))

(define (squer x) (* x x))

(define (make-rat x y) 
  (let ((z (gcd x y)))
      (let  ((a (/ x z))
            (b (/ y z)))
    (if (or (< a 0) (< b 0))
        (cons (- 0 (abs a)) (abs b))
        (cons a b)))))

(define (number x) (car x))

(define (demon x) (cdr x))

(define (print-rat x)
  (display (number x))
  (display "/")
  (display (demon x))
  (newline))




(define (add-rat x y)
  (make-rat (+ (* (number x) (demon y))
               (* (number y) (demon x)))
              
            (* (demon x) (demon y))))
            
            
(define (sub-rat x y)
  (make-rat (- (* (number x) (demon y))
               (* (number y) (demon x)))
              
            (* (demon x) (demon y))))      
            
(define (mul-rat x y)
  (make-rat (* (number x) (number y))
            (* (demon x) (demon y))))
            
(define (div-rat x y)
  (make-rat (* (number x) (demon y))
            (* (number y) (demon x))))
            
(define (equal-rat x y)
  (= (* (number x) (demon y))
     (* (number y) (demon x))))
                        
(define (make-point x y)
  (cons x y))
  
(define (x-point x) (car x))

(define (y-point x) (cdr x))

(define (make-seg x y)    
  (list x y))
  
(define (start-seg x) (car x))

(define (end-seg x) (car (cdr x)))

(define (midpoint-seg x)
  (let ((pa (start-seg x))
        (pb (end-seg x)))
    (make-point (/ (+ (x-point pa)
                      (x-point pb)) 2.0)
                (/ (+ (y-point pa)
                      (y-point pb)) 2.0))))
                      
(define (length-seg x)
  (let ((pa (start-seg x))
        (pb (end-seg x)))
     (sqrt (+ (squer (- (x-point pa)
                        (x-point pb)))
              (squer (- (y-point pa)
                        (y-point pb)))))))

(define (cons-ab a b)        
  (* (expt 2 a) (expt 3 b)))
  
(define (car-ab a)
  (define (iter x y)
    (if (> (remainder y 2) 0) x
        (iter (+ x 1) (/ y 2))))
  (iter 0 a))
  
(define (cdr-ab a)
  (define (iter x y)
    (if (> (remainder y 3) 0) x
        (iter (+ x 1) (/ y 3))))
  (iter 0 a))
                     
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-f a b)
  (lambda (f) (lambda (x) ((a (b f)) x))))
 

 
 
 
 
 
 
             
            
                        
            
            
              