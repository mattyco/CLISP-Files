(defun n-queens (n)
	(print-queens (solve-queens n n) n))

(defun solve-queens (n m)
	(cond ((= n 0) (list nil))
		(t (loop for sol in (solve-queens (- n 1) m)
			append (loop for newCol from 1 to m
						if (loop for row from 1 to n
								for col in sol
							always (/= newCol col (+ col row) (- col row)))
						collect (cons newCol sol))
			)
		)
	)
)

(defun print-queens (lst n)
	(loop for sol in lst
		do (loop for row in sol
			do (loop for i from 1 to n
				do(write (if (= row i) 'Q '[]))
			)
	(terpri))
(terpri))
)

(n-queens 9)
	
