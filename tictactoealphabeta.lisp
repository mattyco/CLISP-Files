
(defun null-board ()
  (list nil nil nil nil nil nil nil nil nil))

(defvar startstate nil)
(setq startstate (null-board))

(defun make-move (pos player move)
  (unless (nth move pos)  
    (let ((b (copy-list pos)))
      (setf (nth move b) player) 
      b)))

(defun movegen (pos player)
  (loop for m from 0 to 8
        unless  (nth m pos)
        collect (make-move pos player m)))
    
(defun won? (pos player)
  (or (and (eq (nth 0 pos) player)  ;top
     (eq (nth 1 pos) player)
     (eq (nth 2 pos) player))
      (and (eq (nth 3 pos) player) ;middle row
     (eq (nth 4 pos) player) 
     (eq (nth 5 pos) player))
      (and (eq (nth 6 pos) player) ;bottom
           (eq (nth 7 pos) player) 
     (eq (nth 8 pos) player))
      (and (eq (nth 0 pos) player) ;left
     (eq (nth 3 pos) player) 
     (eq (nth 6 pos) player))
      (and (eq (nth 1 pos) player) ;middle column
     (eq (nth 4 pos) player) 
     (eq (nth 7 pos) player))
      (and (eq (nth 2 pos) player) ;right
     (eq (nth 5 pos) player) 
     (eq (nth 8 pos) player))
      (and (eq (nth 0 pos) player) ;diagonal down
     (eq (nth 4 pos) player) 
     (eq (nth 8 pos) player))
      (and (eq (nth 2 pos) player) ;diagnonal up
     (eq (nth 4 pos) player) 
     (eq (nth 6 pos) player))))


(defun drawn? (pos)
  (not (member nil pos)))


(defun opposite (player)
  (if (eq player 'x) 'o 'x))

#|
(defun static (pos player)
  (cond ((won? pos player) 
   (+ 1 (/ 1 (length (remove nil pos)))))
  ((won? pos (opposite player)) 
   (- (+ 1 (/ 1 (length (remove nil pos))))))
  (t 0)))

|#


(defun depthcheck (pos depth)
  (declare (ignore depth))
  (or (won? pos 'x) 
      (won? pos 'o)
      (drawn? pos)))

(defun disp-board (b)
  (format t "~% ~d | ~d | ~d   0 1 2~% ~d | ~d | ~d   3 4 5~% ~d | ~d | ~d   6 7 8~%~%"
    (or (nth 0 b) ".") (or (nth 1 b) ".") (or (nth 2 b) ".")
    (or (nth 3 b) ".") (or (nth 4 b) ".") (or (nth 5 b) ".")
    (or (nth 6 b) ".") (or (nth 7 b) ".") (or (nth 8 b) ".")))



(defparameter choices '((0 1 2)
                            (3 4 5)
                            (6 7 8)
                            (0 3 6)
                            (1 4 7)
                            (2 5 8)
                            (0 4 8)
                            (2 4 6)))


(defparameter *max-depth* 4)

(defun depthcheck (pos depth)
  (or (won? pos 'x) 
      (won? pos 'o)
      (drawn? pos)
      (>= depth *max-depth*)))

(defun static (pos player)
  (cond ((won? pos player) 10000)
        ((won? pos (opposite player)) -10000)
        ((drawn? pos) 0)
        (- (iterboard pos player)
           (iterboard pos player))))

(defun iterboard(pos player)
  (loop for line in choices
        summing (line-score pos player line)))

(defun line-score(pos player line)
  (let* ((pieces (loop for point in line 
                      collect (nth point pos)))
        (good (count player pieces)))
    (if (member (opposite player) line)
      0
      good)))

#|

(defun minimax (pos depth player)
  (cond ((depthcheck pos depth)
         (setq static-evals (+ static-evals 1))
	 (list (static pos player)))
	(t
	 (let ((successors (movegen pos player))
	       (best-score -99999)
	       (best-path nil))
	   (cond ((null successors) 
                  (setq static-evals (+ static-evals 1))
                  (list (static pos player)))
		 (t
		  (loop for succ in successors do
		    (let* ((result-succ (minimax succ (+ 1 depth) 
					(opposite player)))
			   (new-value (- (car result-succ))))
		      (when (> new-value best-score)
			(setq best-score new-value)
			(setq best-path  succ ))))
		  (cons best-score best-path)))))))
|#

(defvar static-evals 0)


(defun minimax-a-b (pos depth player)
  (minimax-a-b-1 pos depth player 99999 -99999 t))

(defun minimax-a-b-1 (pos depth player use-thresh pass-thresh return-move)
  (cond ((depthcheck pos depth)
         (setq static-evals (+ static-evals 1))
	 (unless return-move  (static pos player)))
	(t 
	 (let ((successors (movegen pos player))
	       (best-move nil)
               (quit nil)
               (new-value nil))
           (declare (dynamic-extent successors))
	   (cond ((null successors) 
                  (setq static-evals (+ static-evals 1))
                  (unless return-move  (static pos player)))
		 (t
		  (loop for succ in successors 
                        until quit
                        do (setq new-value (- (minimax-a-b-1 succ (+ 1 depth)
						               (opposite player)
						               (- pass-thresh)
						               (- use-thresh)
                                                               nil)))
                             (when (> new-value pass-thresh)
                               (setq pass-thresh new-value)
                               (setq best-move  succ))
                             (when (>= pass-thresh use-thresh) (setq quit t)))
                  (if  return-move best-move pass-thresh)))))))


(defun play (&optional machine-first?)
  (let ((b startstate)
        (next nil))
    (setq static-evals 0)
    (when machine-first? 
      (setq b (minimax-a-b b 0 'o)))
    (do ()
        ((or (won? b 'x) (won? b 'o) (drawn? b))
         (format t "Final position: ~%")
         (disp-board b)
         (cond ((won? b 'o) (format t "computer wins~%"))
               ((won? b 'x) (format t "player wins~%"))
               (t  (format t "Draw~%")))
         static-evals)
      (disp-board b)
      (format t "~a Your move: " static-evals)
      (let ((m (read)))
        (loop until (setq next (make-move b 'x m))
              do (format t "~%~a Illegal move, Try again: " m)
              (setq m (read)))
        (setq b next))              
      (when (and (not (drawn? b))
                 (not (won? b 'o))
                 (not (won? b 'x)))
        (disp-board b)
        (setq b (minimax-a-b b 0 'o))
        (if (and (not (drawn? b))
                 (not (won? b 'o))) 
          (format t "My move: ~%"))
        )
      )
    )
  )



(play)
(print static-evals)
