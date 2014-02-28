;double-recursive (y)
(defun double-recursive (y)
  (cond
    ((null y) nil)
    ((not (listp y)) nil)
    ((null (cdr y)) (list (car y) (car y)))
    (t (append (list (car y) (car y)) (double-recursive (cdr y))))
  )
)


;double-iterative (y)
(defun double-iterative (y)
  (cond
    ((null y) nil)
    ((not (listp y)) nil)
    (t
      (do ((yy y (cdr yy)) (l nil))
	((null yy) l)  					;return condition
	(setq l (append l (list (car yy) (car yy))))
      )
    )
  )
)


;double-mapcar (y)
(defun double-mapcar (y)
  (cond
    ((null y) nil)
    ((not (listp y)) nil)
    (t (apply #'append (mapcar (lambda (x) (list x x)) y)))
  )
)


;rdouble (y)
(defun rdouble (y)
  (cond
    ((null y) nil)
    ((not (listp y)) nil)
    ((and (null (cdr y)) (not (listp (car y)))) (list (car y) (car y)))
    ((and (null (cdr y)) (listp (car y))) (list (rdouble (car y))))
    ((not (listp (car y))) (append (list (car y) (car y)) (rdouble (cdr y))))
    (t (append (list (rdouble (car y))) (rdouble (cdr y))))
  )
)


;my-mapcar (fun l1 l2)
(defun my-mapcar (fun l1 l2)
  (cond
    ((null fun) nil)
    ((null l1) nil)
    ((null l2) nil)
    (t (append (list (funcall fun (car l1) (car l2)))
	  	(my-mapcar fun (cdr l1) (cdr l2))))
  )
)


;rewrite (x)
(defun rewrite (x)
  (cond
    ((null x) nil)
    ((not (listp x)) x)
    
    ((eq (car x) 'if) 
      (cond
	((and (null (cdddr x)) (listp (cadr x))) 
  (list 'cond (list (rewrite (cadr x)) (caddr x))))
	((null (cdddr x)) (list 'cond (cdr x)))
	((listp (cadr x)) 
  (list 'cond (list (rewrite (cadr x)) (caddr x)) (list 't (rewrite (cadddr x)))))
	((listp (caddr x)) 
  (list 'cond (list (cadr x) (rewrite (caddr x))) (list 't (rewrite (cadddr x)))))
	((listp (cadddr x)) 
  (list 'cond (list (cadr x) (caddr x) (rewrite (cadddr x)))))
	(t (list 'cond (list (cadr x) (caddr x)) (list 't (rewrite (cadddr x)))))	
	
      )
    )

    ((not (listp (car x))) (append (list (car x)) (rewrite (cdr x))))
    ((null (cdr x)) (list (rewrite (car x))))
    (t (append (list (rewrite (car x))) (rewrite (cdr x))))
  ) 
)


;check (x)
(defun check (x)
  (cond
    ((equalp (eval x) (eval (rewrite x))) (list 't (eval x)(eval (rewrite x))))
    (t (list 'f (eval x) (eval (rewrite x))))
  )
)









