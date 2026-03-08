(in-package :qiku)

(deftype piece () '(unsigned-byte 4))
(deftype color () '(integer 0 8))
(deftype mailbox-index () '(integer 0 63))

(defconstant +white+ 0)
(defconstant +black+ 8)
(defconstant +empty+ 0)
(defconstant +pawn+  1)
(defconstant +rook+  2)
(defconstant +knight+ 3)
(defconstant +bishop+ 4)
(defconstant +queen+ 5)
(defconstant +king+  6)

(declaim (inline make-piece))
(defun make-piece (color type)
  (declare (type color color)
	   (type piece type))
  (logior color type))

(declaim (inline piece-color))
(defun piece-color (piece)
  (declare (type piece piece))
  (logand piece 8))

(declaim (inline piece-type))
(defun piece-type (piece)
  (declare (type piece piece))
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
   (white-knights #x000000000000042
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

(defun clear-piece-at! (state square)
  (declare (type mailbox-index square))
  (update-bitboard state (piece-at state square) (lognot (ash 1 square)) logand)
  (setf (aref (state-mailbox state) square) +empty+))

(defun set-piece-at! (state square piece)
  (declare (type mailbox-index square)
	   (type piece piece))
  (update-bitboard state piece (ash 1 square) logior)
  (setf (aref (state-mailbox state) square) piece))

