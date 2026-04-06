(in-package :qiku)

(declaim (ftype (function (position color) list) knight-moves))
(defun knight-moves (position color)
  (let ((knights (bb-squares (if (= color +white+) (position-white-knights position) (position-black-knights position)))))
    (mapcan (lambda (square) (knight-moves-from position square color)) knights)))

(defun knight-moves-from (position square color)
  (let* ((piece (piece-at position square))
	 (file (square-file square)))
    (mapcan (lambda (offset)
	      (let* ((to (+ square offset))
		     (to-file (square-file to))
		     (file-difference (abs (- to-file file))))

		(when (and (on-board-p to)
			   (member file-difference '(1 2))
			   (not (square-occupied-by-p position to color)))
		  (let ((target (piece-at position to)))
		    (list
		     (if (/= target +empty+)
			 (make-capture square to piece target)
			 (make-quiet square to piece)))))))
	    +knight-offsets+)))

(defun ray-moves (position square piece color directions)
  "Walk in each direction until blocked"
  (mapcan (lambda (direction)
	    (ray-in-direction position square piece color direction))
	  directions))

(declaim (ftype (function (position mailbox-index piece color cons) list) ray-in-direction))
(defun ray-in-direction (position square piece color direction)
  (let ((dr (car direction))
	(df (cdr direction)))
    (iterate
      (with current-rank = (square-rank square))
      (with current-file = (square-file square))
      (for next-rank = (+ current-rank dr))
      (for next-file = (+ current-file df))
      (for next-square = (+ (* next-rank 8) next-file))
      (while (and (<= 0 next-rank 7) (<= 0 next-file 7)))
      (for target = (piece-at position next-square))

      (setf current-rank next-rank
	    current-file next-file)

      (cond
	((= target +empty+)
	 (collect (make-quiet square next-square piece)))
	((= (piece-color target) (enemy-of color))
	 (collect (make-capture square next-square piece target))
	 (finish))
	(t (finish))))))

(defun rook-moves (position color)
  (let ((rooks (bb-squares (if (= color +white+) (position-white-rooks position) (position-black-rooks position)))))
    (mapcan (lambda (square) (ray-moves position square (piece-at position square) color +rook-directions+)) rooks)))

(defun bishop-moves (position color)
  (let ((bishop (bb-squares (if (= color +white+) (position-white-bishops position) (position-black-bishops position)))))
    (mapcan (lambda (square) (ray-moves position square (piece-at position square) color +bishop-directions+)) bishop)))

(defun queen-moves (position color)
  (let ((queen (bb-squares (if (= color +white+) (position-white-queens position) (position-black-queens position)))))
    (mapcan (lambda (square) (ray-moves position square (piece-at position square) color +queen-directions+)) queen)))

(defun pawn-moves (position color)
  (let* ((direction (if (= color +white+) +8 -8))
	 (start-rank (if (= color +white+) 1 6))
	 (promo-rank (if (= color +white+) 7 0))
	 (pawns (bb-squares (if (= color +white+) (position-white-pawns position) (position-black-pawns position)))))
    (mapcan (lambda (square) (pawn-moves-from position square color direction start-rank promo-rank)) pawns)))

(declaim (ftype (function (position mailbox-index color (integer -8 8) (integer 0 7) (integer 0 7)) list) pawn-moves-from))
(defun pawn-moves-from (position square color direction start-rank promo-rank)
  (let* ((piece (piece-at position square))
	 (push1 (+ square direction))
	 (push2 (+ square direction direction))
	 (rank (square-rank square))
	 (result '()))

    ;; Single and double push
    (when (and (on-board-p push1)
	       (not (square-occupied-p position push1)))
      (if (= (square-rank push1) promo-rank)
	  (setf result (append result (promotion-moves square push1 piece nil color)))
	  (push (make-quiet square push1 piece) result))

      (when (and (= rank start-rank)
		 (on-board-p push2)
		 (not (square-occupied-p position push2)))
	(push (make-move :from square :to push2 :piece piece :flags +double-pawn-push-flag+) result)))

    ;; Captures
    (iterate
      (with capture-squares = (pawn-capture-squares square direction))
      (for capture-square in capture-squares)

      (when (on-board-p capture-square)
	(let ((target (piece-at position capture-square)))
	  (when (and (/= target +empty+)
		     (= (piece-color target) (enemy-of color)))
	    (if (= (square-rank capture-square) promo-rank)
		(setf result (append result (promotion-moves square capture-square piece target color)))
		(push (make-capture square capture-square piece target) result))))))

    ;; En passant
    (let ((ep (position-ep-square position)))
      (when (and ep
		 (member ep (pawn-capture-squares square direction)))
	(let ((captured (piece-at position (- ep direction))))
	  (push (make-move :from square :to ep
			   :piece piece :captured
			   captured :flags (logior +capture-flag+ +en-passant-flag+))
		result))))
    result))

(declaim (ftype (function (mailbox-index (integer -8 8)) list) pawn-capture-squares))
(defun pawn-capture-squares (square direction)
  (let ((file (square-file square)))
    (list (if (> file 0) (+ square direction -1) -1)
	  (if (< file 7) (+ square direction +1) -1))))

(defun promotion-moves (from to piece captured color)
  (mapcar (lambda (type)
	    (make-promotion from to piece captured type color))
	  '(#.+rook+ #.+knight+ #.+bishop+ #.+queen+)))


(defun king-moves (position color)
  (let ((king-square (king-square position color)))
    (append
     (king-step-moves position king-square color)
     (castling-moves position king-square color))))

(declaim (ftype (function (position mailbox-index color) list) king-step-moves))
(defun king-step-moves (position square color)
  (let* ((piece (piece-at position square))
	 (file (square-file square)))
    (mapcan (lambda (offset)
	      (let* ((to (+ square offset))
		     (to-file (square-file to))
		     (file-difference (abs (- to-file file))))

		(when (and (on-board-p to)
			   (<= file-difference 1)
			   (not (square-occupied-by-p position to color)))
		  (let ((target (piece-at position to)))
		    (list
		     (if (/= target +empty+)
			 (make-capture square to piece target)
			 (make-quiet square to piece)))))))
	    +king-offsets+)))

(declaim (ftype (function (position mailbox-index color) list) castling-moves))
(defun castling-moves (position square color)
  (let ((rights (position-castling-rights position))
	(piece (piece-at position square))
        (result '()))

    (when (= color +white+)
      ;; White kingside (K): squares 5, 6 must be empty
      (when (and (logbitp 3 rights)
                 (not (square-occupied-p position 5))
                 (not (square-occupied-p position 6))
                 (not (king-in-check-p position color))
                 (not (square-attacked-p position 5 color))
                 (not (square-attacked-p position 6 color)))
        (push (make-move :from square :to 6 :piece piece :flags +castling-flag+)
              result))
      ;; White queenside (Q): squares 1, 2, 3 must be empty
      (when (and (logbitp 2 rights)
                 (not (square-occupied-p position 1))
                 (not (square-occupied-p position 2))
                 (not (square-occupied-p position 3))
                 (not (king-in-check-p position color))
                 (not (square-attacked-p position 3 color))
                 (not (square-attacked-p position 2 color)))
        (push (make-move :from square :to 2 :piece piece :flags +castling-flag+)
              result)))

    (when (= color +black+)
      ;; Black kingside (k): squares 61, 62 must be empty
      (when (and (logbitp 1 rights)
                 (not (square-occupied-p position 61))
                 (not (square-occupied-p position 62))
                 (not (king-in-check-p position color))
                 (not (square-attacked-p position 61 color))
                 (not (square-attacked-p position 62 color)))
        (push (make-move :from square :to 62 :piece piece :flags +castling-flag+)
              result))
      ;; Black queenside (q): squares 57, 58, 59 must be empty
      (when (and (logbitp 0 rights)
                 (not (square-occupied-p position 57))
                 (not (square-occupied-p position 58))
                 (not (square-occupied-p position 59))
                 (not (king-in-check-p position color))
                 (not (square-attacked-p position 58 color))
                 (not (square-attacked-p position 59 color)))
        (push (make-move :from square :to 58 :piece piece :flags +castling-flag+)
              result)))

    result))

(declaim (inline king-in-check-p))
(defun king-in-check-p (position color)
  (square-attacked-p position (king-square position color) color))

(defun square-attacked-p (position square defender-color)
  (let* ((attacker-color (enemy-of defender-color))
	 (color-index (if (= attacker-color +white+) 1 0))
	 (enemy-pawns (enemy-bb position defender-color +pawn+))
	 (enemy-knights (enemy-bb position defender-color +knight+))
	 (enemy-king (enemy-bb position defender-color +king+)))
    (declare (type (unsigned-byte 64) enemy-pawns enemy-knights enemy-king))
    
    (or
     (not (zerop (logand (aref +pawn-attacks+ color-index square) enemy-pawns)))
     (not (zerop (logand (aref +knight-attacks+ square) enemy-knights)))
     (some (lambda (dir)
	     (ray-finds-attacker-p position square attacker-color dir
				   (list +rook+ +queen+)))
	   +rook-directions+)
     (some (lambda (dir)
	     (ray-finds-attacker-p position square attacker-color dir
				   (list +bishop+ +queen+)))
	   +bishop-directions+)

     (not (zerop (logand (aref +king-attacks+ square) enemy-king))))))

(declaim (ftype (function (position mailbox-index color cons list) boolean) ray-finds-attacker-p))
(defun ray-finds-attacker-p (position from-square attacker-color direction attacker-types)
  (let ((dr (car direction))
	(df (cdr direction)))
    (iterate
      (with rank = (square-rank from-square))
      (with file = (square-file from-square))
      (setf rank (+ rank dr)
	    file (+ file df))
      (while (and (<= 0 rank 7) (<= 0 file 7)))
      (let* ((target (+ (* rank 8) file))
	     (piece (piece-at position target)))
	(cond
	  ((and (= (piece-color piece) attacker-color)
		(member (piece-type piece) attacker-types))
	   (return t))
	  ((zerop piece) nil)
	  (t (return nil)))))))


;; Generate all moves
(defun generate-pseudolegal-moves (position)
  (let ((color (position-turn position)))
    (append
     (pawn-moves position color)
     (knight-moves position color)
     (bishop-moves position color)
     (rook-moves position color)
     (queen-moves position color)
     (king-moves position color))))

(defun generate-legal-moves (position)
  (let ((color (position-turn position)))
    (iterate
      (for move in (generate-pseudolegal-moves position))
      (do-move! position move)
      (when (not (king-in-check-p position color))
        (collect move))
      (undo-move! position move))))

(defun perft (position depth)
  (if (= depth 0)
      1
      (let ((color (position-turn position)))
        (iterate
          (for move in (generate-pseudolegal-moves position))
          (do-move! position move)
          (unless (king-in-check-p position color)
            (sum (perft position (1- depth))))
          (undo-move! position move)))))
