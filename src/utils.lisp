;;; Big file of crap so I can keep the implementation cleaner
(in-package :qiku)

;; Bitboard and state utils
(defun generate-piece (piece-list)
  (iterate
    (for p in piece-list)
    (for piece = (case p
		   (bp (make-piece :black :pawn ))
		   (wp (make-piece :white :pawn ))
		   (br (make-piece :black :rook ))
		   (wr (make-piece :white :rook ))
		   (bn (make-piece :black :knight ))
		   (wn (make-piece :white :knight ))
		   (bb (make-piece :black :bishop ))
		   (wb (make-piece :white :bishop ))
		   (bq (make-piece :black :queen ))
		   (wq (make-piece :white :queen ))
		   (bk (make-piece :black :king ))
		   (wk (make-piece :white :king ))
		   (otherwise (make-piece :white :empty))))
    (collect piece result-type (simple-array (unsigned-byte 4) (64)))))

(defmacro update-bitboard (state piece bit op)
  `(case (piece-color ,piece)
     (#.+white+
      (case (piece-type ,piece)
	(#.+pawn+   (setf (state-white-pawns ,state) (,op (state-white-pawns ,state) ,bit)))
        (#.+rook+   (setf (state-white-rooks ,state) (,op (state-white-rooks ,state) ,bit)))
        (#.+knight+ (setf (state-white-knights ,state) (,op (state-white-knights ,state) ,bit)))
        (#.+bishop+ (setf (state-white-bishops ,state) (,op (state-white-bishops ,state) ,bit)))
        (#.+queen+  (setf (state-white-queens ,state) (,op (state-white-queens ,state) ,bit)))
        (#.+king+   (setf (state-white-king ,state) (,op (state-white-king ,state) ,bit)))
	(otherwise nil)))
     (#.+black+
      (case (piece-type ,piece)
        (#.+pawn+   (setf (state-black-pawns ,state) (,op (state-black-pawns ,state) ,bit)))
        (#.+rook+   (setf (state-black-rooks ,state) (,op (state-black-rooks ,state) ,bit)))
        (#.+knight+ (setf (state-black-knights ,state) (,op (state-black-knights ,state) ,bit)))
        (#.+bishop+ (setf (state-black-bishops ,state) (,op (state-black-bishops ,state) ,bit)))
        (#.+queen+  (setf (state-black-queens ,state) (,op (state-black-queens ,state) ,bit)))
        (#.+king+   (setf (state-black-king ,state) (,op (state-black-king ,state) ,bit)))))
     (otherwise nil)))

(declaim (inline piece-at))
(defun piece-at (state square)
  (declare (type mailbox-index square))
  (aref (state-mailbox state) square))

;; Move related utils
(defun make-quiet (from to piece)
  (make-move :from from :to to :piece piece))

(defun make-capture (from to piece captured)
  (make-move :from from :to to :piece piece
             :captured captured :flags +capture-flag+))

(defun make-promotion (from to piece captured promo-type color)
  (make-move :from from :to to :piece piece
             :captured captured
             :promotion (make-piece color promo-type)
             :flags (logior +promotion-flag+
                            (if captured +capture-flag+ 0))))

(declaim (inline square-occupied-p))
(defun square-occupied-p (state square)
  (/= +empty+ (piece-at state square)))

(declaim (inline square-occupied-by-p))
(defun square-occupied-by-p (state square color)
  (declare (type mailbox-index square)
	   (type color color))
  (let ((piece (piece-at state square)))
    (and (not (zerop piece))
	 (= color (piece-color (piece-at state square))))))

(declaim (inline enemy-of))
(defun enemy-of (color)
  (declare (type color color))
  (logxor color 8))

(declaim (inline square-rank))
(defun square-rank (square)
  (declare (type mailbox-index square))
  (nth-value 0 (floor square 8)))

(declaim (inline square-file))
(defun square-file (square)
  (mod square 8))

(declaim (inline on-board-p))
(defun on-board-p (square)
  (<= 0 square 63))

(defun king-square (state color)
  (declare (type color color))
  (let ((bb (if (= color +white+) (state-white-king state) (state-black-king state))))
    (declare (type (unsigned-byte 64) bb))
    (1- (integer-length bb))))

(declaim (inline bb-squares))
(declaim (ftype (function ((unsigned-byte 64)) (cons))))
(defun bb-squares (bb)
  (iterate
    (for b first bb then (logand b (1- b)))
    (declare (type (unsigned-byte 64) b))
    (until (zerop b))
    (collect (1- (integer-length (logand b (- b)))))))

;; Formatting related utils
(defun square->algebraic (square)
  "Convert a square index (0=a1) to a string like \"e4\"."
  (let ((file (square-file square))
        (rank (square-rank square)))
    (format nil "~c~d"
            (aref "abcdefgh" file)
            (1+ rank))))

(defun piece-type-name (piece)
  (case (piece-type piece)
    (#.+pawn+   "Pawn")
    (#.+rook+   "Rook")
    (#.+knight+ "Knight")
    (#.+bishop+ "Bishop")
    (#.+queen+  "Queen")
    (#.+king+   "King")
    (otherwise  "Empty")))

(defun piece-color-name (piece)
  (case (piece-color piece)
    (#.+white+ "White")
    (#.+black+ "Black")
    (otherwise "None")))

(defun piece-name (piece)
  (if (zerop piece)
      "Empty"
      (format nil "~a ~a" (piece-color-name piece) (piece-type-name piece))))

(defmethod print-object ((m move) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "(~a->~a) (~a~@[ captures ~a~]~@[ promotes to ~a~])"
            (square->algebraic  (move-from m))
            (square->algebraic  (move-to   m))
            (piece-name     (move-piece m))
            (when (move-captured  m) (piece-name (move-captured  m)))
            (when (move-promotion m) (piece-name (move-promotion m))))))


(defun checkmate-p (state)
  (and (king-in-check-p state (state-turn state))
       (null (generate-legal-moves state))))

(defun stalemate-p (state)
  (and (not (king-in-check-p state (state-turn state)))
       (null (generate-legal-moves state))))
