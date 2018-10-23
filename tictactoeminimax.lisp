(defvar static-evals 0)
(defvar startboardpos nil)

(defun newboard ()
	(list nil nil nil nil nil nil nil nil nil))
(setq startboardpos (newboard))

(defun disp-board (b)
  (format t "~% ~d | ~d | ~d    0 1 2 ~% ~d | ~d | ~d    3 4 5 ~% ~d | ~d | ~d    6 7 8~%~%"
	  (or (nth 0 b) ".") (or (nth 1 b) ".") (or (nth 2 b) ".")
	  (or (nth 3 b) ".") (or (nth 4 b) ".") (or (nth 5 b) ".")
	  (or (nth 6 b) ".") (or (nth 7 b) ".") (or (nth 8 b) ".")
))

(defun make-move (boardpos player move)
  (unless (nth move boardpos)  
    (let ((b (copy-list boardpos)))
      (setf (nth move b) player) 
      b)))

(defun generate (boardpos player)
  (loop for m from 0 to 8
        unless  (nth m boardpos)
        collect (make-move boardpos player m)))
		

(defun won? (boardpos player)
	(or 
		(and 
			(eq (nth 0 boardpos) player) 
			(eq (nth 1 boardpos) player)
			(eq (nth 2 boardpos) player))
		(and 
			(eq (nth 3 boardpos) player) 
			(eq (nth 4 boardpos) player) 
			(eq (nth 5 boardpos) player))
		(and 
			(eq (nth 6 boardpos) player) 
			(eq (nth 7 boardpos) player) 
			(eq (nth 8 boardpos) player))
		(and 
			(eq (nth 0 boardpos) player) 
			(eq (nth 3 boardpos) player) 
			(eq (nth 6 boardpos) player))
		(and 
			(eq (nth 1 boardpos) player) 
			(eq (nth 4 boardpos) player) 
			(eq (nth 7 boardpos) player))
		(and 
			(eq (nth 2 boardpos) player) 
			(eq (nth 5 boardpos) player) 
			(eq (nth 8 boardpos) player))
		(and 
			(eq (nth 0 boardpos) player)
			(eq (nth 4 boardpos) player) 
			(eq (nth 8 boardpos) player))
		(and 
			(eq (nth 2 boardpos) player) 
			(eq (nth 4 boardpos) player) 
			(eq (nth 6 boardpos) player))))

(defun draw? (boardpos)
	(not (member nil boardpos)))

(defun toggleplayer(player)
	(if (eq player 'x)
		'o
		'x))

(defun depthcheck (boardpos depth)
	(declare (ignore boardpos))
	(if (>= depth 4)
		t
		nil))

(defparameter choices '((0 1 2)
                            (3 4 5)
                            (6 7 8)
                            (0 3 6)
                            (1 4 7)
                            (2 5 8)
                            (0 4 8)
                            (2 4 6)))
;heuristic

(defun iterboard (boardpos player)
	(loop for line in choices
		summing (line-score boardpos player line)))

(defun line-score(boardpos player line)
  (let* ((pieces (loop for point in line 
                      collect (nth point boardpos)))
        (good (count player pieces)))
    (if (member (toggleplayer player) line)
      0
      good)))

(defun evaluate (boardpos player)
  (cond ((won? boardpos player) 10000)
        ((won? boardpos (toggleplayer player)) -10000)
        ((draw? boardpos) 0)
        (- (iterboard boardpos player)
           (iterboard boardpos player))))

#|

(defun evaluate (boardpos player)
  (cond ((won? boardpos player) 
   (+ 1 (/ 1 (length (remove nil boardpos)))))
  ((won? boardpos (toggleplayer player)) 
   (- (+ 1 (/ 1 (length (remove nil boardpos))))))
  (t 0)))
|#

(defun minimax (boardpos depth player)
	(cond ((depthcheck boardpos depth)
	 (setq static-evals (+ static-evals 1))
	 (list (evaluate boardpos player)))

	(t
	 (let ((boardpossibleMoves (generate boardpos player))
				 (value -999)
				 (path nil))
		 (cond ((null boardpossibleMoves)
				(setq static-evals (+ static-evals 1))
				(list (evaluate boardpos player)))
		 (t
			(loop for move in boardpossibleMoves 
				do
				(let* ((newMove (minimax move (+ 1 depth) 
					(toggleplayer player)))
				 (newValue (- (car newMove))))
					(when (> newValue value)
			(setq value newValue)
			(setq path  move ))))
			(cons value path)
			)
		 )
		 )
	 )

	)
	)

(defun play ()
	(let ((State startboardpos)
		(next nil))
		(setq static-evals 0)
		(do ()
			((or (won? State 'x) (won? State 'o) (draw? State))
				(disp-board State)
				(cond ((won? State 'o) (format t "~%Computer Wins~%"))
						((won? State 'x) (format t "~%Player Wins~%"))
						(t  (format t "~%Draw~%")))
				static-evals)
				(disp-board State)
			(format t "~%Your move: ")
			(let ((m (read)))
				(loop until (setq next (make-move State 'x m))
							do (format t "~% Illegal move~%")
							(setq m (read)))
				(setq State next))              
			(when (and (not (draw? State))
								 (not (won? State 'o))
								 (not (won? State 'x)))
				(setq State (cdr (minimax State 0 'o)))
				(if (and (not (draw? State))
					(not (won? State 'o))) 
					(format t "My move: ~%"))
			)
		)
		)
	)


(play)
(print static-evals)

