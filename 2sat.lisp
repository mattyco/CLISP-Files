  
(defun ask (ls)
(cond ((null ls) '())
((not (symboli (car ls))) (cons (car ls) (ask (cdr ls))))
(t (ask (cdr ls)))))

(defun flatten (l)
(cond ((null l) nil)
((atom (car l)) (cons (car l) (flatten (cdr l))))
(t (append (flatten (car l)) (flatten (cdr l))))))

(defun symboli (ob)
(cond ((eq ob '!) t)
((eq ob 'V) t)
((eq ob '&) t)
(t nil)))

(defun symbolj (ob)
(cond ((eq ob '!) 'not)
((eq ob 'V) 'or)
((eq ob '&) 'and)
(t nil)))

(defun tras (ls os)
(cond ( (no-var (flatten ls)) (when (not ( eq (pr (calc (nott ls)) (flatten (nott ls))) nil))  (print (mapcar (lambda (x y) (list x y)) lsf (pr (calc (nott ls)) (flatten  ls)))) (incf counter)))


;(print (pr (calc (nott ls)) (flatten  ls)))))
(t (tras (asg ls (car os) t) (cdr os))
(tras (asg ls (car os) nil) (cdr os)))))


(defun no-var (ls)
(cond ((null ls) t)
((is-var (car ls)) nil)               
(t (no-var (cdr ls)))))

(defun is-var (ob)
(if (or (symboli ob) (eq ob t) (eq ob nil)) nil t)) 

(defun pr (o ls)
(if (eq o t) (pr1 ls))) 


(defun pr1 (ls)
(cond ((null ls) '())
((not (symboli (car ls))) (cons (car ls) (pr1 (cdr ls))))
(t (pr1 (cdr ls)))))



(defun asg (ls ob ob1)
(cond ((null ls) '())
((listp (car ls)) (cons (asg1 (car ls) ob ob1) (asg (cdr ls) ob ob1)))
(t (cons (car ls) (asg (cdr ls) ob ob1))))) 

(defun asg1 (ls ob ob1)
(cond ((null ls) '())
((eq (car ls) ob) (cons ob1 (asg1 (cdr ls) ob ob1)))
(t (cons (car ls) (asg1 (cdr ls) ob ob1)))))

(defun nott (ls)
(cond ((null ls) '())
((listp (car ls)) (cons (nott1 (car ls)) (nott (cdr ls) )))
(t (cons (car ls) (nott (cdr ls)))))) 

(defun nott1 (ls)
(cond ((null ls) '())
((eq (car ls) '!) (cons (op (cadr ls)) (nott1 (cddr ls))))
(t ( cons (car ls) (nott1 (cdr ls))))))

(defun op (ob)
(if (eq ob t) nil t))



(defun calc (ls)
(cond ((null ls) t)
(t (and (or (caar ls) (caddar ls)) (calc (cddr ls))))))


(defun main ()
(setf ls (read))
(setf counter 0)
(setf lsf (ask (flatten ls)))
(tras ls (remove-duplicates (ask (flatten ls))))
(if (= counter 0) 'Unsatisfiable))

(main)


 																																		
