(defvar static-evals 0)
(defvar startpos nil)

;This method creates the initial board as a combination of nulls
(defun newboard ()
	(list nil nil nil nil nil nil nil nil nil))

;Initialize 'startpos' as newboard()
(setq startpos (newboard))

;This method displays the current state of the board; replace with dot if not filled
(defun disp-board (b)
  (format t "~% ~d ~d ~d   0 1 2~% ~d ~d ~d   3 4 5~% ~d ~d ~d   6 7 8~%~%"
	  (or (nth 0 b) ".") (or (nth 1 b) ".") (or (nth 2 b) ".")
	  (or (nth 3 b) ".") (or (nth 4 b) ".") (or (nth 5 b) ".")
	  (or (nth 6 b) ".") (or (nth 7 b) ".") (or (nth 8 b) ".")
))

;This method is used to play a move; pos is the current state
(defun make-move (pos player move)
  (unless (nth move pos)  				;unless nth position is not nil do
    (let ((b (copy-list pos)))
      (setf (nth move b) player) 		;sets the player symbol to a selected position
      b)))								;return b

;This method generates all possible moves from a particular position of the board
(defun generate (pos player)			
  (loop for m from 0 to 8
        unless  (nth m pos)
        collect (make-move pos player m)))		;collects everything in a container (list)
		

;This method returns true if a player has won
(defun won? (pos player)					
	(or 
		(and 
			(eq (nth 0 pos) player) 
			(eq (nth 1 pos) player)
			(eq (nth 2 pos) player))
		(and 
			(eq (nth 3 pos) player) 
			(eq (nth 4 pos) player) 
			(eq (nth 5 pos) player))
		(and 
			(eq (nth 6 pos) player) 
			(eq (nth 7 pos) player) 
			(eq (nth 8 pos) player))
		(and 
			(eq (nth 0 pos) player) 
			(eq (nth 3 pos) player) 
			(eq (nth 6 pos) player))
		(and 
			(eq (nth 1 pos) player) 
			(eq (nth 4 pos) player) 
			(eq (nth 7 pos) player))
		(and 
			(eq (nth 2 pos) player) 
			(eq (nth 5 pos) player) 
			(eq (nth 8 pos) player))
		(and 
			(eq (nth 0 pos) player)
			(eq (nth 4 pos) player) 
			(eq (nth 8 pos) player))
		(and 
			(eq (nth 2 pos) player) 
			(eq (nth 4 pos) player) 
			(eq (nth 6 pos) player))))


;This method tests if any positions are nil, draw if not
(defun draw? (pos)
	(not (member nil pos)))

;This method 'switches the player'
(defun opposite(player)
	(if (eq player 'x)
		'o
		'x))

;This method checks the depth==3 for n-ply look ahead (here n=3)
(defun depthcheck (pos depth)		
	(declare (ignore pos))			;defines a macro	;ignore is a lambda expression which will ignore pos 
	(if (>= depth 3)
		t
		nil))

;Combination of all diagonals, rows and columns
(defparameter choices '((0 1 2)
                            (3 4 5)
                            (6 7 8)
                            (0 3 6)
                            (1 4 7)
                            (2 5 8)
                            (0 4 8)
                            (2 4 6)))

;This method does the heuristic calculation at n depth
(defun iterboard (pos player)
	(loop for line in choices
		summing (line-score pos player line)))

;This method helps calculate heuristic - checks 'lines' on which only one player's token is present, returns its count
(defun line-score(pos player line)
  (let* ((pieces (loop for point in line 				;let* does all the assignments sequentially
                      collect (nth point pos)))
        (good (count player pieces)))
    (if (member (opposite player) line)
      0
      good)))

;This method evaluates heuristic of a state; +/-10000 is taken as +/-inf
(defun evaluate (pos player)
  (cond ((won? pos player) 10000)
        ((won? pos (opposite player)) -10000)
        ((draw? pos) 0)
        (- (iterboard pos player)
           (iterboard pos player))))

;This method runs the minimax algorithm
(defun minimax (pos depth player) 			
	(cond ((depthcheck pos depth)						;minimax wait until the programme parses to the ply depth-=
	 (list (evaluate pos player)))
	(t
	 (let ((possibleMoves (generate pos player))
				 (value -999)
				 (path nil))
		 (cond ((null possibleMoves) 			    ;no possible moves to generate? => heuristic value of the position is checked
				(list (evaluate pos player)))			
		 (t
			(loop for move in possibleMoves 
				do
				(let* ((newMove (minimax move (+ 1 depth) 	;minimax on the possible postions with toggled player
					(opposite player)))
					(newValue (- (car newMove))))		;newvalue is the heuristic value of the subsequent recursions
					(when (> newValue value)			;the heuristic values are compared and backtracked
						(setq value newValue)
						(setq path move))))
				(cons value path)
			)
		 	)
		)
	 )
	)
)

;This method is our main()
(defun play ()
	(let ((State startpos)
			(next nil))
		(do ()															;infinite loop breaking on return state
			((or (won? State 'x) (won? State 'o) (draw? State))
				(disp-board State)
				(cond ((won? State 'o) (format t "~%Computer Wins~%"))
						((won? State 'x) (format t "~%Player Wins~%"))
						(t  (format t "~%Draw~%"))))
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
					(format t "My move: ~%"))))))

;Function call to play game
(play)

