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

(defun ray-moves (state square piece color directions)
  "Walk in each direction until blocked"
  (mapcan (lambda (direction)
	    (ray-in-direction state square piece color direction))
	  directions))

(defun ray-in-direction (state square piece color direction)
  (let ((dr (car direction))
	(df (cdr direction)))
    (iterate
      (with current-rank = (square-rank square))
      (with current-file = (square-file square))
      (for next-rank = (+ current-rank dr))
      (for next-file = (+ current-file df))
      (for next-square = (+ (* next-rank 8) next-file))
      (while (and (<= 0 next-rank 7) (<= 0 next-file 7)))
      (for target = (piece-at state next-square))

      (setf current-rank next-rank
	    current-file next-file)

      (cond
	((= target +empty+)
	 (collect (make-quiet square next-square piece)))
	((= (piece-color target) (enemy-of color))
	 (collect (make-capture square next-square piece target))
	 (finish))
	(t (finish))))))

(defun rook-moves (state color)
  (let ((rooks (bb-squares (if (= color +white+) (white-rooks state) (black-rooks state))))
	(rook-directions '((1 . 0) (-1 . 0) (0 . 1) (0 . -1))))
    (mapcan (lambda (square) (ray-moves state square (piece-at state square) color rook-directions)) rooks)))

(defun bishop-moves (state color)
  (let ((bishop (bb-squares (if (= color +white+) (white-bishops state) (black-bishops state))))
	(bishop-directions '((1 . 1) (1 . -1) (-1 . 1) (-1 . -1))))
    (mapcan (lambda (square) (ray-moves state square (piece-at state square) color bishop-directions)) bishop)))

(defun queen-moves (state color)
  (let ((queen (bb-squares (if (= color +white+) (white-queens state) (black-queens state))))
	(queen-directions '((1 . 1) (1 . -1) (-1 . 1) (-1 . -1) (1 . 0) (-1 . 0) (0 . 1) (0 . -1))))
    (mapcan (lambda (square) (ray-moves state square (piece-at state square) color queen-directions)) queen)))
