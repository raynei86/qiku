(in-package :qiku)

(deftype piece () '(unsigned-byte 4))
(deftype color () '(integer 0 8))
(deftype mailbox-index () '(integer 0 63))

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
(defstruct position
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

(defmacro update-bitboard (position piece bit op)
  `(case (piece-color ,piece)
     (#.+white+
      (case (piece-type ,piece)
	(#.+pawn+   (setf (position-white-pawns ,position) (,op (position-white-pawns ,position) ,bit)))
        (#.+rook+   (setf (position-white-rooks ,position) (,op (position-white-rooks ,position) ,bit)))
        (#.+knight+ (setf (position-white-knights ,position) (,op (position-white-knights ,position) ,bit)))
        (#.+bishop+ (setf (position-white-bishops ,position) (,op (position-white-bishops ,position) ,bit)))
        (#.+queen+  (setf (position-white-queens ,position) (,op (position-white-queens ,position) ,bit)))
        (#.+king+   (setf (position-white-king ,position) (,op (position-white-king ,position) ,bit)))
	(otherwise nil)))
     (#.+black+
      (case (piece-type ,piece)
        (#.+pawn+   (setf (position-black-pawns ,position) (,op (position-black-pawns ,position) ,bit)))
        (#.+rook+   (setf (position-black-rooks ,position) (,op (position-black-rooks ,position) ,bit)))
        (#.+knight+ (setf (position-black-knights ,position) (,op (position-black-knights ,position) ,bit)))
        (#.+bishop+ (setf (position-black-bishops ,position) (,op (position-black-bishops ,position) ,bit)))
        (#.+queen+  (setf (position-black-queens ,position) (,op (position-black-queens ,position) ,bit)))
        (#.+king+   (setf (position-black-king ,position) (,op (position-black-king ,position) ,bit)))))
     (otherwise nil)))

(declaim (ftype (function (position mailbox-index) t) clear-piece-at!))
(defun clear-piece-at! (position square)
  (update-bitboard position (piece-at position square) (lognot (ash 1 square)) logand)
  (setf (aref (position-mailbox position) square) +empty+))

(declaim (ftype (function (position mailbox-index piece) t) set-piece-at!))
(defun set-piece-at! (position square piece)
  (update-bitboard position piece (ash 1 square) logior)
  (setf (aref (position-mailbox position) square) piece))
