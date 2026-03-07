(in-package :qiku)

(defun knight-moves (state color)
  (let ((knights (bb-squares (if (= color +white+) (white-knights state) (black-knights state)))))
    (mapcan (lambda (square) (knight-moves-from state square color)) knights)))

(defun knight-moves-from (state square color)
  (let* ((piece (piece-at state square))
	 (file (square-file square))
	 (offsets '(+17 +15 +10 +6 -6 -10 -15 -17)))
    (mapcan (lambda (offset)
	      (let* ((to (+ square offset))
		     (to-file (square-file to))
		     (file-difference (abs (- to-file file))))

		(when (and (on-board-p to)
			   (member file-difference '(1 2))
			   (not (square-occupied-by-p state to color)))
		  (let ((target (piece-at state to)))
		    (list
		     (if (/= target +empty+)
			 (make-capture square to piece target)
			 (make-quiet square to piece)))))))
	    offsets)))
