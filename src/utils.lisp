;;; Big file of crap so I can keep the implementation cleaner
(in-package :qiku)

;; Piece related utils
(declaim (ftype (function (state mailbox-index) piece) piece-at)
	 (inline piece-at))
(defun piece-at (state square)
  (aref (state-mailbox state) square))

;; Move related utils
(declaim (ftype (function (mailbox-index mailbox-index piece) move) make-quiet))
(defun make-quiet (from to piece)
  (make-move :from from :to to :piece piece))

(declaim (ftype (function (mailbox-index mailbox-index piece piece) move) make-capture))
(defun make-capture (from to piece captured)
  (make-move :from from :to to :piece piece
             :captured captured :flags +capture-flag+))

(declaim (ftype (function (mailbox-index mailbox-index piece (or null piece) (or null piece) color) move) make-promotion))
(defun make-promotion (from to piece captured promo-type color)
  (make-move :from from :to to :piece piece
             :captured captured
             :promotion (make-piece color promo-type)
             :flags (logior +promotion-flag+
                            (if captured +capture-flag+ 0))))

;; Board related utils
(declaim (ftype (function (state mailbox-index) boolean) square-occupied-p)    (inline square-occupied-p))
(defun square-occupied-p (state square)
  (/= +empty+ (piece-at state square)))

(declaim (ftype (function (state mailbox-index color) boolean) square-occupied-by-p)
	 (inline square-occupied-by-p))
(defun square-occupied-by-p (state square color)
  (let ((piece (piece-at state square)))
    (and (not (zerop piece))
	 (= color (piece-color (piece-at state square))))))

(declaim (ftype (function (color) color) enemy-of)
	 (inline enemy-of))
(defun enemy-of (color)
  (logxor color 8))

(declaim (ftype (function (state color piece) (unsigned-byte 64)) enemy-bb))
(defun enemy-bb (state friendly-color bb-type)
  (let ((enemy-color (enemy-of friendly-color)))
    (if (= enemy-color +white+)
	(case bb-type
	  (#.+pawn+ (state-white-pawns state))
	  (#.+rook+ (state-white-rooks state))
	  (#.+knight+ (state-white-knights state))
	  (#.+bishop+ (state-white-bishops state))
	  (#.+queen+ (state-white-queens state))
	  (#.+king+ (state-white-king state)))
	(case bb-type
	  (#.+pawn+ (state-black-pawns state))
	  (#.+rook+ (state-black-rooks state))
	  (#.+knight+ (state-black-knights state))
	  (#.+bishop+ (state-black-bishops state))
	  (#.+queen+ (state-black-queens state))
	  (#.+king+ (state-black-king state))))))

(declaim (ftype (function (mailbox-index) (integer 0 7)) square-rank))
(defun square-rank (square)
  (declare (type mailbox-index square))
  (nth-value 0 (floor square 8)))

(declaim (ftype (function (fixnum) mailbox-index) square-file))
(defun square-file (square)
  (mod square 8))

(defun on-board-p (square)
  (<= 0 square 63))

(declaim (ftype (function (state color) mailbox-index) king-square))
(defun king-square (state color)
  (let ((bb (if (= color +white+) (state-white-king state) (state-black-king state))))
    (declare (type (unsigned-byte 64) bb))
    (when (zerop bb) (return-from king-square 0))
    (1- (integer-length bb))))

(declaim (ftype (function ((unsigned-byte 64)) (cons))))
(defun bb-squares (bb)
  (iterate
    (for b first bb then (logand b (1- b)))
    (until (zerop b))
    (collect (1- (integer-length (logand b (- b)))))))

(defun checkmate-p (state &optional (moves t))
  (and (king-in-check-p state (state-turn state))
       (null moves)))

(defun stalemate-p (state &optional (moves t))
  (and (not (king-in-check-p state (state-turn state)))
       (null moves)))

(declaim (inline adjacent-files-mask)
	 (ftype (function ((integer 0 7)) (unsigned-byte 64)) adjacent-files-mask))
(defun adjacent-files-mask (file)
  (logior (if (> file 0) (aref +file-masks+ (1- file)) 0)
	  (if (< file 7) (aref +file-masks+ (1+ file)) 0)))

(declaim (inline adjacent-ranks-mask)
	 (ftype (function ((integer 0 7)) (unsigned-byte 64)) adjacent-ranks-mask))
(defun adjacent-ranks-mask (rank)
  (logior (if (> rank 0) (aref +rank-masks+ (1- rank)) 0)
	  (if (< rank 7) (aref +rank-masks+ (1+ rank)) 0)))

(declaim (inline distance)
	 (ftype (function (mailbox-index mailbox-index) mailbox-index) distance))
(defun distance (square1 square2)
  (max
   (abs (- (square-rank square2) (square-rank square1)))
   (abs (- (square-file square2) (square-file square1)))))

;; Formatting related utils
(defun square->algebraic (square)
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


