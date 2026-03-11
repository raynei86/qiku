(in-package :qiku)

(deftype piece () '(unsigned-byte 4))
(deftype color () '(integer 0 8))
(deftype mailbox-index () '(integer 0 63))

(serapeum:defconst +white+ 0)
(serapeum:defconst +black+ 8)
(serapeum:defconst +empty+ 0)
(serapeum:defconst +pawn+  1)
(serapeum:defconst +rook+  2)
(serapeum:defconst +knight+ 3)
(serapeum:defconst +bishop+ 4)
(serapeum:defconst +queen+ 5)
(serapeum:defconst +king+  6)

(declaim (ftype (function (color piece) piece) make-piece)
	 (inline make-piece))
(defun make-piece (color type)
  (logior color type))

(declaim (ftype (function (piece) color) piece-color)
	 (inline piece-color))
(defun piece-color (piece)
  (logand piece 8))

(declaim (ftype (function (piece) piece) piece-type)
	 (inline piece-type))
(defun piece-type (piece)
  (logand piece 7))

;; More commonly known as "position", but that's a reserved name
(defstruct state
  (mailbox (generate-piece
	    '(wr wn wb wq wk wb wn wr
	      wp wp wp wp wp wp wp wp
	      00 00 00 00 00 00 00 00
	      00 00 00 00 00 00 00 00
	      00 00 00 00 00 00 00 00
	      00 00 00 00 00 00 00 00
	      bp bp bp bp bp bp bp bp
	      br bn bb bq bk bb bn br))
    :type (simple-array (unsigned-byte 4) (64)))
   
   ;; Whole bunch of bitboards
   (white-pawns #x000000000000FF00
    :type (unsigned-byte 64))
   (white-knights #x0000000000000042
    :type (unsigned-byte 64))
   (white-bishops  #x000000000000024
    :type (unsigned-byte 64))
   (white-rooks #x000000000000081
    :type (unsigned-byte 64))
   (white-queens #x0000000000000008
    :type (unsigned-byte 64))
   (white-king #x0000000000000010
    :type (unsigned-byte 64))
   (black-pawns #x00ff000000000000
    :type (unsigned-byte 64))
   (black-knights #x4200000000000000
    :type (unsigned-byte 64))
   (black-bishops #x2400000000000000
    :type (unsigned-byte 64))
   (black-rooks #x8100000000000000
    :type (unsigned-byte 64))
   (black-queens #x0800000000000000
    :type (unsigned-byte 64))
   (black-king #x1000000000000000
    :type (unsigned-byte 64))

   ;; Other misc. things
   (turn +white+ :type color)
   (castling-rights #b1111 :type (unsigned-byte 4))
   (ep-square nil :type (or null (integer 0 63)))
   (halfmove-clock 0 :type (integer 0 *))
   (fullmove-number 1 :type (integer 1)))

(defun generate-piece (piece-list)
  (declare (type cons piece-list))
  (iterate
    (for p in piece-list)
    (for piece = (case p
		   (bp (make-piece +black+ +pawn+ ))
		   (wp (make-piece +white+ +pawn+ ))
		   (br (make-piece +black+ +rook+ ))
		   (wr (make-piece +white+ +rook+ ))
		   (bn (make-piece +black+ +knight+ ))
		   (wn (make-piece +white+ +knight+ ))
		   (bb (make-piece +black+ +bishop+ ))
		   (wb (make-piece +white+ +bishop+ ))
		   (bq (make-piece +black+ +queen+ ))
		   (wq (make-piece +white+ +queen+ ))
		   (bk (make-piece +black+ +king+ ))
		   (wk (make-piece +white+ +king+ ))
		   (otherwise (make-piece +white+ +empty+))))
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

(declaim (ftype (function (state mailbox-index) t) clear-piece-at!))
(defun clear-piece-at! (state square)
  (update-bitboard state (piece-at state square) (lognot (ash 1 square)) logand)
  (setf (aref (state-mailbox state) square) +empty+))

(declaim (ftype (function (state mailbox-index piece) t) set-piece-at!))
(defun set-piece-at! (state square piece)
  (update-bitboard state piece (ash 1 square) logior)
  (setf (aref (state-mailbox state) square) piece))
