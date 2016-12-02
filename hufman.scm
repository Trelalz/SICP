(define p '((a 19) (b 9) (c 8) (d 1) (e 10) (f 5) (g 6) (h 7)))

(define l '((leaf d 1) (leaf f 5)
 (leaf g 6)
 (leaf h 7)
 (leaf c 8)
 (leaf b 9)
 (leaf e 10)
 (leaf a 19)))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
  
(define (leaf? a)
  (eq? (car a) 'leaf))
  
(define (symbol-leaf a)
  (cadr a))
  
(define (weight-leaf a)
  (caddr a))
  
(define (make-code-tree left right)
  (list left right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))
        
(define (symbols a)
  (if (leaf? a)
      (list (symbol-leaf a))
      (caddr a)))
      
(define (weight a)
  (if (leaf? a)
      (weight-leaf a)
      (cadddr a)))
      
(define (decode bits t)
  (define (de-code bit current)
    (if (null? bit) '()
      (let ((next (choose (car bit) current)))
        (if (leaf? next) 
          (cons (symbol-leaf next)
               (de-code (cdr bit) t))
       (de-code (cdr bit) next)))))
  (de-code bits t))

(define (choose a tree)
  (cond ((= a 0) (car tree))
        ((= a 1) (cadr tree))
        (else (errors "unknow bits --CHOOSE  " a))))
        
(define (errors a b)
  (if (eq? b #!void) (pp a)
      (errors b (display a))))
              
(define (adjoin-set x set )
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) 
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs) '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf
                        (car pair)
                        (cadr pair))
                    (make-leaf-set 
                       (cdr pairs))))))
        

(define (hufman-tree ls)
  (if (= (length ls) 2) ls
        (hufman-tree
        (adjoin-set (make-code-tree (car ls) (cadr ls)) (cddr ls)))))
        
(define (make-pair s)
  (define (iter a b)
    (if (null? a) b
        (iter (cdr a) (add-f (car a) b))))
  (iter s '()))

(define (add-f a b)
    (cond ((null? b) (cons (list a 1) b))
        ((eq? a (caar b)) (cons (list (caar b) (+ (cadar b) 1)) (cdr b)))
        (else (cons (car b) (add-f a (cdr b))))))
        
      
(define (table t)
  (define (iter a b)
    (cond ((null? a) b)
          ((leaf? a) 
           (cons (list (symbol-leaf a) '()) b))
          (else (append (map (lambda(x) (list (car x) (cons 0 (cadr x)))) (iter (car a) b))
                (map (lambda(x) (list (car x) (cons 1 (cadr x)))) (iter (cadr a) b))))))
   (iter t '()))
               
(define (encode s)
  (let ((tree (hufman-tree (make-leaf-set (make-pair s)))))
    (let ((codes (table tree)))
      (define (iter a b)
        (if (null? a) b
          (iter (cdr a) (append b (code (car a) codes)))))
  (list (iter s '()) tree))))
   

(define (code a s)
  (cond ((null? s)
         (errors "unknow error--CODE" s))
        ((eq? a (caar s)) (cadar s))
        (else (code a (cdr s)))))















