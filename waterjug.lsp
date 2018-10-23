(defun get-first-jug (state)
	(car state))

(defun get-second-jug (state)
	(cadr state))

(defun fill-first-jug (state x)
	(list x (get-second-jug state))
)

(defun fill-second-jug (state y)
	(list (get-first-jug state) y)
)

(defun empty-first-jug (state)
	(list 0 (get-second-jug state))
)

(defun empty-second-jug (state)
	(list (get-first-jug state) 0)
)

(defun pour-first-jug (state x)
	(if (> (- x (get-first-jug state)) (get-second-jug state))
		(list (+ (get-first-jug state) (get-second-jug state)) 0)
		(list x (- (get-second-jug state) (- x (get-first-jug state))))
	)
)

(defun pour-second-jug (state y)
	(if (< (get-first-jug state) (- y (get-second-jug state)))
		(list 0 (+ (get-first-jug state) (get-second-jug state)))
		(list (- (get-first-jug state) (- y (get-second-jug state))) y)
	)
)

(defun child-nodes(state x y)
	(list 
		(fill-first-jug state x)
		(fill-second-jug state y)
		(empty-first-jug state)
		(empty-second-jug state)
		(pour-first-jug state x)
		(pour-second-jug state y)
	)
)

(defun rem-dups (list1 list2 list3)
	(del-dups (del-dups list1 list2) list3))

(defun del-dups (list1 list2)
	(if (equal list1 '())
		'()
		(if (= (ismember (car list1) list2) 1) 
			(del-dups (cdr list1) list2)
			(cons (car list1) (del-dups (cdr list1) list2))
		)
	)
)


(defun ismember (elem list1)
	(if (null list1)
		0
		(if (equal elem (car list1))
			1
			(ismember elem (cdr list1))
		)
	)
)
		
	
;(defun ismember (list1 list2 list3)
;	(if (or (equal (member list1 list2) nil) (equal (member list1 list3) nil))
;		0
;		1
;	)
;)

;(defun ismember (list1 list2 list3)
;	(if (or (equal (inloop list1 list3) 0)
;	 (equal (inloop list1 list2) 0))
;		1
;		0
;	)
;)

;(defun outloop (list1 list2)
;	(if (null list1)
;		'()
;		(if (= (inloop (car list1) list2) 0)
;			(outloop (cdr list1) list2)
;			(append (car list1) (outloop (cdr list1) list2))
;		)
;	)
;)

;(defun inloop (elem list2)
;	(cond 
;		((null list2) 1)
;		((not (equal elem (car list2))) (inloop elem (cdr list2)))
;		(t (return-from inloop 0))
;	)
;)
	
(defun DFS (x y goal-state)
	(write (trav '((0 0)) '() x y goal-state))
) 

	
;(defun trav (open closed x y goal-state)
;	(if (equal (car open) goal-state)
;		(car open)
;		(if (equal (ismember (car open) (cdr open) closed) 1) 
;			(trav (cdr open) closed x y goal-state)
;			(append (list (car open)) (trav (append (child-nodes (car open) x y) (cdr open)) (cons (car open) closed) x y goal-state))
;		)
;	)
;)

(defun trav (open closed x y goal-state)
	(if (equal (car open) goal-state)
		(car open)
		(cons (car open) 
			(trav (append (del-dups (child-nodes (car open) x y) (cdr open) (cons (car open) closed)) (cdr open)) closed x y goal-state))
		
	)
)

(defun BFS (x y goal-state)
	(write (travBFS '((0 0)) '() x y goal-state)))

(defun travBFS (open closed x y goal-state)
	(if (equal (car open) goal-state)
		(list (car open))
		(cons (car open) (travBFS 
							(append (rem-dups (child-nodes (car open) x y) open (cons (car open) closed)) 
								(cdr open)) (cons (car open) closed) x y goal-state) 
		)
	)
)  
  

;(write (rem-dups (child-nodes '(0 0) 3 2) '((0 0)) '()))
(BFS 3 8 '(0 4))

(DFS 3 8 '(0 4))

;(write (del-dups '((0 0) (1 0)) '((1 0) (0 0))))

	
	
	
