(in-package :qiku)

(defconstant +knight-offsets+ '(+17 +15 +10 +6 -6 -10 -15 -17))
(defconstant +king-offsets+ '(+9 +8 +7 +1 -1 -7 -8 -9))
(defconstant +rook-directions+   '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
(defconstant +bishop-directions+ '((1 . 1) (1 . -1) (-1 . 1) (-1 . -1)))
(defconstant +queen-directions+  (append +rook-directions+ +bishop-directions+))

(defparameter *knight-attacks*
  (iterate
    (with table = (make-array 64 :element-type '(unsigned-byte 64) :initial-element 0))
    (for square to 63)
    (setf (aref table square)
	  (iterate
	    (for offset in +knight-offsets+)
	    (for to = (+ square offset))
	    (when (and (on-board-p to)
		       (<= (abs (- (logand to 7) (logand square 7))) 2))
	      (sum (ash 1 to)))))
    (finally (return table))))

(defparameter *king-attacks*
  (iterate
    (with table = (make-array 64 :element-type '(unsigned-byte 64) :initial-element 0))
    (for square to 63)
    (setf (aref table square)
	  (iterate
	    (for offset in +king-offsets+)
	    (for to = (+ square offset))
	    (when (and (on-board-p to)
		       (<= (abs (- (logand to 7) (logand square 7))) 1))
	      (sum (ash 1 to)))))
    (finally (return table))))

(defparameter *pawn-attacks*
  (iterate
    (with table = (make-array '(2 64) :element-type '(unsigned-byte 64) :initial-element 0))
    (for square to 63)
    (setf (aref table 0 square)
	  (iterate
	    (for offset in '(7 9))
	    (for to = (+ square offset))
	    (when (and (< square 56)
		       (<= (abs (- (logand to 7) (logand square 7))) 1))
	      (sum (ash 1 to)))))
    (setf (aref table 1 square)
	  (iterate
	    (for offset in '(-7 -9))
	    (for to = (+ square offset))
	    (when (and (< square 56)
		       (<= (abs (- (logand to 7) (logand square 7))) 1))
	      (sum (ash 1 to)))))
    (finally (return table))))


(defun knight-moves (state color)
  (declare (type color color))
  (let ((knights (bb-squares (if (= color +white+) (state-white-knights state) (state-black-knights state)))))
    (mapcan (lambda (square) (knight-moves-from state square color)) knights)))

(defun knight-moves-from (state square color)
  (let* ((piece (piece-at state square))
	 (file (square-file square)))
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
	    +knight-offsets+)))

(defun ray-moves (state square piece color directions)
  "Walk in each direction until blocked"
  (mapcan (lambda (direction)
	    (ray-in-direction state square piece color direction))
	  directions))

(defun ray-in-direction (state square piece color direction)
  (declare (type mailbox-index square)
	   (type piece piece)
	   (type color color)
	   (type cons direction))
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
  (let ((rooks (bb-squares (if (= color +white+) (state-white-rooks state) (state-black-rooks state)))))
    (mapcan (lambda (square) (ray-moves state square (piece-at state square) color +rook-directions+)) rooks)))

(defun bishop-moves (state color)
  (let ((bishop (bb-squares (if (= color +white+) (state-white-bishops state) (state-black-bishops state)))))
    (mapcan (lambda (square) (ray-moves state square (piece-at state square) color +bishop-directions+)) bishop)))

(defun queen-moves (state color)
  (let ((queen (bb-squares (if (= color +white+) (state-white-queens state) (state-black-queens state)))))
    (mapcan (lambda (square) (ray-moves state square (piece-at state square) color +queen-directions+)) queen)))

(defun pawn-moves (state color)
  (let* ((direction (if (= color +white+) +8 -8))
	 (start-rank (if (= color +white+) 1 6))
	 (promo-rank (if (= color +white+) 7 0))
	 (pawns (bb-squares (if (= color +white+) (state-white-pawns state) (state-black-pawns state)))))
    (mapcan (lambda (square) (pawn-moves-from state square color direction start-rank promo-rank)) pawns)))

(defun pawn-moves-from (state square color direction start-rank promo-rank)
  (declare (type mailbox-index square)
	   (type color color)
	   (type (integer -8 +8) direction)
	   (type (integer 0 7) start-rank promo-rank))
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
	  (push (make-quiet square push1 piece) result))

      (when (and (= rank start-rank)
		 (on-board-p push2)
		 (not (square-occupied-p state push2)))
	(push (make-move :from square :to push2 :piece piece :flags +double-pawn-push-flag+) result)))

    ;; Captures
    (iterate
      (with capture-squares = (pawn-capture-squares square direction))
      (for capture-square in capture-squares)

      (when (on-board-p capture-square)
	(let ((target (piece-at state capture-square)))
	  (when (and (/= target +empty+)
		     (= (piece-color target) (enemy-of color)))
	    (if (= (square-rank capture-square) promo-rank)
		(setf result (append result (promotion-moves square capture-square piece target color)))
		(push (make-capture square capture-square piece target) result))))))

    ;; En passant
    (let ((ep (state-ep-square state)))
      (when (and ep
		 (member ep (pawn-capture-squares square direction)))
	(let ((captured (piece-at state (- ep direction))))
	  (push (make-move :from square :to ep
			   :piece piece :captured
			   captured :flags (logior +capture-flag+ +en-passant-flag+))
		result))))
    result))

(defun pawn-capture-squares (square direction)
  (declare (type mailbox-index square)
	   (type (integer -8 +8) direction))
  (let ((file (square-file square)))
    (list (if (> file 0) (+ square direction -1) -1)
	  (if (< file 7) (+ square direction +1) -1))))

(defun promotion-moves (from to piece captured color)
  (mapcar (lambda (type)
	    (make-promotion from to piece captured type color))
	  '(+rook+ +knight+ +bishop+ +queen+)))


(defun king-moves (state color)
  (let ((king-square (king-square state color)))
    (append
     (king-step-moves state king-square color)
     (castling-moves state king-square color))))

(defun king-step-moves (state square color)
  (declare (type mailbox-index square)
	   (type color color))
  (let* ((piece (piece-at state square))
	 (file (square-file square)))
    (mapcan (lambda (offset)
	      (let* ((to (+ square offset))
		     (to-file (square-file to))
		     (file-difference (abs (- to-file file))))

		(when (and (on-board-p to)
			   (<= file-difference 1)
			   (not (square-occupied-by-p state to color)))
		  (let ((target (piece-at state to)))
		    (list
		     (if (/= target +empty+)
			 (make-capture square to piece target)
			 (make-quiet square to piece)))))))
	    +king-offsets+)))

(defun castling-moves (state square color)
  (declare (type mailbox-index square)
	   (type color color))
  (let ((rights (state-castling-rights state))
	(piece (piece-at state square))
        (result '()))

    (when (= color +white+)
      ;; White kingside (K): squares 5, 6 must be empty
      (when (and (logbitp 3 rights)
                 (not (square-occupied-p state 5))
                 (not (square-occupied-p state 6))
                 (not (king-in-check-p state color))
                 (not (square-attacked-p state 5 color))
                 (not (square-attacked-p state 6 color)))
        (push (make-move :from square :to 6 :piece piece :flags +castling-flag+)
              result))
      ;; White queenside (Q): squares 1, 2, 3 must be empty
      (when (and (logbitp 2 rights)
                 (not (square-occupied-p state 1))
                 (not (square-occupied-p state 2))
                 (not (square-occupied-p state 3))
                 (not (king-in-check-p state color))
                 (not (square-attacked-p state 3 color))
                 (not (square-attacked-p state 2 color)))
        (push (make-move :from square :to 2 :piece piece :flags +castling-flag+)
              result)))

    (when (= color +black+)
      ;; Black kingside (k): squares 61, 62 must be empty
      (when (and (logbitp 1 rights)
                 (not (square-occupied-p state 61))
                 (not (square-occupied-p state 62))
                 (not (king-in-check-p state color))
                 (not (square-attacked-p state 61 color))
                 (not (square-attacked-p state 62 color)))
        (push (make-move :from square :to 62 :piece piece :flags +castling-flag+)
              result))
      ;; Black queenside (q): squares 57, 58, 59 must be empty
      (when (and (logbitp 0 rights)
                 (not (square-occupied-p state 57))
                 (not (square-occupied-p state 58))
                 (not (square-occupied-p state 59))
                 (not (king-in-check-p state color))
                 (not (square-attacked-p state 58 color))
                 (not (square-attacked-p state 59 color)))
        (push (make-move :from square :to 58 :piece piece :flags +castling-flag+)
              result)))

    result))

(declaim (inline king-in-check-p))
(defun king-in-check-p (state color)
  (square-attacked-p state (king-square state color) color))

(defun square-attacked-p (state square defender-color)
  (let* ((attacker-color (enemy-of defender-color))
	 (color-index (if (= attacker-color +white+) 1 0))
	 (enemy-pawns (enemy-bb state defender-color +pawn+))
	 (enemy-knights (enemy-bb state defender-color +knight+))
	 (enemy-king (enemy-bb state defender-color +king+)))
    (declare (type (unsigned-byte 64) enemy-pawns enemy-knights enemy-king))
    
    (or
     (not (zerop (logand (aref *pawn-attacks* color-index square) enemy-pawns)))
     (not (zerop (logand (aref *knight-attacks* square) enemy-knights)))
     (some (lambda (dir)
	     (ray-finds-attacker-p state square attacker-color dir
				   (list +rook+ +queen+)))
	   +rook-directions+)
     (some (lambda (dir)
	     (ray-finds-attacker-p state square attacker-color dir
				   (list +bishop+ +queen+)))
	   +bishop-directions+)

     (not (zerop (logand (aref *king-attacks* square) enemy-king))))))

(defun ray-finds-attacker-p (state from-square attacker-color direction attacker-types)
  (declare (type mailbox-index from-square)
	   (type color attacker-color)
	   (type cons direction)
	   (type list attacker-types))
  (let ((dr (car direction))
	(df (cdr direction)))
    (iterate
      (with rank = (square-rank from-square))
      (with file = (square-file from-square))
      (setf rank (+ rank dr)
	    file (+ file df))
      (while (and (<= 0 rank 7) (<= 0 file 7)))
      (let* ((target (+ (* rank 8) file))
	     (piece (piece-at state target)))
	(cond
	  ((and (= (piece-color piece) attacker-color)
		(member (piece-type piece) attacker-types))
	   (return t))
	  ((zerop piece) nil)
	  (t (return nil)))))))


;; Generate all moves
(defun generate-pseudolegal-moves (state)
  (let ((color (state-turn state)))
    (append
     (pawn-moves state color)
     (knight-moves state color)
     (bishop-moves state color)
     (rook-moves state color)
     (queen-moves state color)
     (king-moves state color))))

(defun generate-legal-moves (state)
  (let ((color (state-turn state)))
    (iterate
      (for move in (generate-pseudolegal-moves state))
      (do-move! state move)
      (when (not (king-in-check-p state color))
        (collect move))
      (undo-move! state move))))

(defun perft (state depth)
  (if (= depth 0)
      1
      (let ((color (state-turn state)))
        (iterate
          (for move in (generate-pseudolegal-moves state))
          (do-move! state move)
          (unless (king-in-check-p state color)
            (sum (perft state (1- depth))))
          (undo-move! state move)))))
