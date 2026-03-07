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

(defun pawn-moves (state color)
  (let* ((direction (if (= color +white+) +8 -8))
	 (start-rank (if (= color +white+) 1 6))
	 (promo-rank (if (= color +white+) 7 0))
	 (pawns (bb-squares (if (= color +white+) (white-pawns state) (black-pawns state)))))
    (mapcan (lambda (square) (pawn-moves-from state square color direction start-rank promo-rank)) pawns)))

(defun pawn-moves-from (state square color direction start-rank promo-rank)
  (let* ((piece (piece-at state square))
	 (push1 (+ square direction))
	 (push2 (+ square direction direction))
	 (rank (square-rank square))
	 (result '()))

    ;; Single and double push
    (when (and (on-board-p push1)
	       (not (square-occupied-p state push1)))
      (if (= (square-rank push1) promo-rank)
	  (setf result (append result (promotion-moves square push1 piece nil color)))
	  (push (make-quiet square push1 piece) result)))

    (when (and (= rank start-rank)
	       (on-board-p push2)
	       (not (square-occupied-p state push2)))
      (push (make-move :from square :to push2 :piece piece :flags +double-pawn-push-flag+) result))

    ;; Captures
    (iterate
      (with capture-squares = (pawn-capture-squares square direction))
      (for capture-square in capture-squares)
      (while (on-board-p capture-square))
      (for target = (piece-at state capture-square))

      (when (and (/= target +empty+)
		 (= (piece-color target) (enemy-of color)))
	(if (= (square-rank capture-square) promo-rank)
	    (setf result (append result (promotion-moves square capture-square piece target color)))
	    (push (make-capture square capture-square piece target) result))))

    ;; En passant
    (let ((ep (ep-square state)))
      (when (and ep
		 (member ep (pawn-capture-squares square color)))
	(let ((captured (piece-at state (- ep direction))))
	  (push (make-move :from square :to ep
			   :piece piece :captured
			   captured :flags (logior +capture-flag+ +en-passant-flag+))
		result))))
    result))

(defun pawn-capture-squares (square direction)
  (let ((file (square-file square)))
    (list (if (> file 0) (+ square direction -1) -1)
	  (if (< file 7) (+ square direction +1) -1))))

(defun promotion-moves (from to piece captured color)
  (mapcar (lambda (type)
	    (make-promotion from to piece captured type color))
	  '(:rook :knight :bishop :queen)))
