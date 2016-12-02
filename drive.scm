        
(define (variable? x)
  (symbol? x))
  
(define (same-variable va vb)
  (and (variable? va) (variable? vb) (eq? va vb)))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))
  
(define (expontation? e)
  (and (pair? e) (eq? (car e) '**)))

(define (base e)
  (cadr e))
  
(define (exponten e)
  (caddr e))
  
(define (make-expontation a b)
  (cond ((=number b 0) 1)
        ((=number b 1) a)
        (else (list '** a b))))

(define (product? e)
  (and (pair? e) (eq? (car e) '*)))

(define (adden e)
  (cadr e))
  
(define (augen e)
  (caddr e))

(define (multiper e) (cadr e))

(define (multipend e) (caddr e))

(define (errors x y)
  (if (pair? y)
  (error y (display x))))

(define (drive exps var)
  (cond ((number? exps) 0)
        ((variable? exps)
         (if (same-variable exps var) 1 0))
        ((sum? exps) 
         (make-sum (drive (adden exps) var)
                   (drive (augen exps) var)))
        ((product? exps)
         (make-sum 
           (make-product (multiper exps)
                         (drive (multipend exps) var))
           (make-product (drive (multiper exps) var)
                         (multipend exps))))            
        ((expontation? exps) 
            (make-product (exponten exps)
              (make-expontation (base exps) (- (exponten exps) 1))))
        (else (errors "Unknow exps" exps))))


(define (make-sum a b)
 (cond ((=number a 0) b)
       ((=number b 0) a)
       ((and (number? a) (number? b)) (+ a b))
       (else (list '+ a b))))

(define (make-product a b) 
  (cond ((or (=number a 0) (=number b 0)) 0)
        ((=number a 1) b)
        ((=number b 1) a)
        (else (list '* a b))))
        
(define (=number x y)
  (and (number? x) (= x y)))

(define (mem-of-set? mem set)
  (cond ((null? set) #f)
        ((equal? mem (car set)) #t)
        (else (mem-of-set? mem (cdr set)))))

(define (adjon x set)
  (if (mem-of-set? x set) set
      (cons x set)))

(define (inter-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((mem-of-set? (car s1) s2) (cons (car s1) (inter-set (cdr s1) s2)))
        (else (inter-set (cdr s1) s2))))

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((mem-of-set? (car s1) s2) (union-set (cdr s1) s2))
        (else (cons (car s1) (union-set (cdr s1) s2)))))

(define (mem-of-tree? x tree)
  (cond ((null? tree) #f)
        ((= x (entry tree)) #t)
        ((< x (entry tree))
         (mem-of-tree? x (left-banch tree)))
        (else (mem-of-tree? x (right-banch tree)))))

(define (entry tree)
  (car tree))
  
(define (left-banch tree)
  (cadr tree))
  
(define (right-banch tree)
  (caddr tree))
  
(define (make-tree entry left right)
  (list entry left right))

(define (adjon-tree x tree)
  (cond ((null? tree) (make-tree x '() '()))
        ((= x (entry tree)) tree)
        ((> x (entry tree))
         (make-tree
           (entry tree)
           (left-banch tree)
           (adjon-tree x (right-banch tree))))
        (else (make-tree
           (entry tree)
           (adjon-tree x (left-banch tree))
           (right-banch tree)))))


;(define (banenth-tree s n)
;  (if (= n 0) (cons '() s)
;      (let ((left (quotient (- n 1) 2))))))


























