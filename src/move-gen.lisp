(in-package :qiku)

(defconstant +knight-offsets+ '(+17 +15 +10 +6 -6 -10 -15 -17))
(defconstant +king-offsets+ '(+9 +8 +7 +1 -1 -7 -8 -9))
(defconstant +rook-directions+   '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
(defconstant +bishop-directions+ '((1 . 1) (1 . -1) (-1 . 1) (-1 . -1)))
(defconstant +queen-directions+  (append +rook-directions+ +bishop-directions+))

(defun knight-moves (state color)
  (let ((knights (bb-squares (if (= color +white+) (white-knights state) (black-knights state)))))
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
  (let ((rooks (bb-squares (if (= color +white+) (white-rooks state) (black-rooks state)))))
    (mapcan (lambda (square) (ray-moves state square (piece-at state square) color +rook-directions+)) rooks)))

(defun bishop-moves (state color)
  (let ((bishop (bb-squares (if (= color +white+) (white-bishops state) (black-bishops state)))))
    (mapcan (lambda (square) (ray-moves state square (piece-at state square) color +bishop-directions+)) bishop)))

(defun queen-moves (state color)
  (let ((queen (bb-squares (if (= color +white+) (white-queens state) (black-queens state)))))
    (mapcan (lambda (square) (ray-moves state square (piece-at state square) color +queen-directions+)) queen)))

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


(defun king-moves (state color)
  (let ((king-square (king-square state color)))
    (append
     (king-step-moves state king-square color)
     (castling-moves state king-square color))))

(defun king-step-moves (state square color)
  (let* ((piece (piece-at state square))
	 (file (square-file square))
	 (offsets '(+9 +8 +7 +1 -1 -7 -8 -9)))
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
	    offsets)))

(defun castling-moves (state square color)
  (let ((rights (castling-rights state))
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

(defun king-in-check-p (state color)
  (square-attacked-p state (king-square state color) color))

(defun square-attacked-p (state square defender-color)
  (let ((attacker-color (enemy-of defender-color)))
    (or
     ;; Look for enemy pawns on the squares that would attack SQ.
     (let* ((dir (if (= attacker-color +white+) +8 -8))
            (f   (square-file square))
            (candidates
             (remove-if
              #'null
              (list (when (> f 0) (- square dir -1))  ; pawn to left
                    (when (< f 7) (- square dir +1))  ; pawn to right
                    ))))
       (some (lambda (from)
               (and (on-board-p from)
                    (let ((p (piece-at state from)))
                      (and (= (piece-color p) attacker-color)
                           (= (piece-type  p) +pawn+)))))
             candidates))

     (some (lambda (offset)
             (let* ((from   (+ square offset))
                    (f-diff (abs (- (square-file from) (square-file square)))))
               (and (on-board-p from)
                    (member f-diff '(1 2))
                    (let ((p (piece-at state from)))
                      (and (= (piece-color p) attacker-color)
                           (= (piece-type  p) +knight+))))))
           +knight-offsets+)

     (some (lambda (dir)
             (ray-finds-attacker-p state square attacker-color dir
                                   (list +rook+ +queen+)))
           +rook-dirs+)

     (some (lambda (dir)
             (ray-finds-attacker-p state square attacker-color dir
                                   (list +bishop+ +queen+)))
           +bishop-dirs+)

     (some (lambda (offset)
             (let* ((from   (+ square offset))
                    (f-diff (abs (- (square-file from) (square-file square)))))
               (and (on-board-p from)
                    (<= f-diff 1)
                    (let ((p (piece-at state from)))
                      (and (= (piece-color p) attacker-color)
                           (= (piece-type  p) +king+))))))
           +king-offsets+))))

(defun ray-finds-attacker-p (state from-square attacker-color dir attacker-types)
  "Walk a ray from FROM-SQ; return T if the first piece found is an attacker."
  (let ((dr (car dir))
        (df (cdr dir)))
    (labels ((walk (cur-square)
               (let* ((nr (+ (square-rank cur-square) dr))
                      (nf (+ (square-file cur-square) df)))
                 (if (or (< nr 0) (> nr 7) (< nf 0) (> nf 7))
                     nil
                     (let* ((next-square (+ (* nr 8) nf))
                            (p       (piece-at state next-square)))
                       (cond
                         ((= p +empty+) (walk next-square))
                         ((and (= (piece-color p) attacker-color)
                               (member (piece-type p) attacker-types))
                          t)
                         (t nil)))))))
      (walk from-square))))


;; Generate all moves
(defun generate-pseudolegal-moves (state)
  (let ((color (turn state)))
    (append
     (pawn-moves state color)
     (knight-moves state color)
     (bishop-moves state color)
     (rook-moves state color)
     (queen-moves state color)
     (king-moves state color))))

(defun generate-legal-moves (state)
  (remove-if
   (lambda (move) (king-in-check-p (do-move state move) (turn state)))
   (generate-pseudolegal-moves state)))
