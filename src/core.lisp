(in-package :qiku)

(defconstant +white+ -1)
(defconstant +black+ 1)
(defconstant +empty+ 0)
(defconstant +pawn+ 1)
(defconstant +rook+ 2)
(defconstant +knight+ 3)
(defconstant +bishop+ 4)
(defconstant +queen+ 5)
(defconstant +king+ 6)

(deftype piece () '(unsigned-byte 4))
(deftype color () '(unsigned-byte 1))

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
    (let ((color (if (eql color :white) +white+ +black+))
	  (type (case type
		  (:pawn +pawn+)
		  (:rook +rook+)
		  (:knight +knight+)
		  (:bishop +bishop+)
		  (:queen +queen+)
		  (:king +king+)
		  (:empty +empty+))))
    (logior color type)))

(declaim (inline piece-color))
(defun piece-color (piece)
  (logand piece 8))

(declaim (inline piece-type))
(defun piece-type (piece)
  (logand piece 7))

;; More commonly known as "position", but that's a reserved name
(defclass state ()
  ((mailbox
    :initarg :mailbox
    :accessor mailbox
    :initform (generate-piece
	       '(wr wn wb wq wk wb wn wr
		 wp wp wp wp wp wp wp wp
		 00 00 00 00 00 00 00 00
		 00 00 00 00 00 00 00 00
		 00 00 00 00 00 00 00 00
		 00 00 00 00 00 00 00 00
		 bp bp bp bp bp bp bp bp
		 br bn bb bq bk bb bn br))
    :type (simple-array (:dimension 64 :element-type piece))
    :documentation "The 8x8 mailbox representation of the board")
   
   ;; Whole bunch of bitboards
   (white-pawns   :initarg :white-pawns
		  :accessor white-pawns
		  :type (unsigned-byte 64)
		  :initform #x000000000000FF00)
   (white-knights :initarg :white-knights
		  :accessor white-knights
		  :type (unsigned-byte 64)
		  :initform #x000000000000042)
   (white-bishops :initarg :white-bishops
		  :accessor white-bishops
		  :type (unsigned-byte 64)
		  :initform #x000000000000024)
   (white-rooks   :initarg :white-rooks
		  :accessor white-rooks
		  :type (unsigned-byte 64)
		  :initform #x000000000000081)
   (white-queens  :initarg :white-queens
		  :accessor white-queens
		  :type (unsigned-byte 64)
		  :initform #x0000000000000008)
   (white-king    :initarg :white-king
		  :accessor white-king
		  :type (unsigned-byte 64)
		  :initform #x0000000000000010)
   (black-pawns   :initarg :black-pawns
		  :accessor black-pawns
		  :type (unsigned-byte 64)
		  :initform #x00ff000000000000)
   (black-knights :initarg :black-knights
		  :accessor black-knights
		  :type (unsigned-byte 64)
		  :initform #x4200000000000000)
   (black-bishops :initarg :black-bishops
		  :accessor black-bishops
		  :type (unsigned-byte 64)
		  :initform #x2400000000000000)
   (black-rooks   :initarg :black-rooks
		  :accessor black-rooks
		  :type (unsigned-byte 64)
		  :initform #x8100000000000000)
   (black-queens  :initarg :black-queens
		  :accessor black-queens
		  :type (unsigned-byte 64)
		  :initform #x0800000000000000)
   (black-king    :initarg :black-king
		  :accessor black-king
		  :type (unsigned-byte 64)
		  :initform #x1000000000000000)
   
   ;; Other misc. things
   (turn
    :initarg :turn
    :accessor turn
    :type color
    :initform +white+
    :documentation "The side that is moving")
   (castling-rights
    :initarg :castling-rights
    :accessor castling-rights
    :type (unsigned-byte 4)
    :initform #b1111
    :documentation "Castling rights represented as a bitmask (white is first): KQkq")
   (ep-square :initarg :ep-square
	      :accessor ep-square
	      :initform nil
	      :type (or null (integer 0 63)))
   (halfmove-clock :initarg :halfmove-clock
		   :accessor halfmove-clock
		   :initform 0
		   :type (integer))
   (fullmove-number :initarg :fullmove-number
		    :accessor fullmove-number
		    :initform 1
		    :type (integer 1))))

(defmethod white-pieces ((state state))
  (logior (white-pawns state) (white-knights state) (white-bishops state)
          (white-rooks state) (white-queens state) (white-king state)))

(defmethod black-pieces ((state state))
  (logior (black-pawns state) (black-knights state) (black-bishops state)
          (black-rooks state) (black-queens state) (black-king state)))

(defmethod all-pieces ((state state))
  (logior (white-pieces state) (black-pieces state)))

(defun clear-piece-at! (state square)
  (update-bitboard state (piece-at state square) (lognot (ash 1 square)) logand)
  (setf (aref (mailbox state) square) +empty+))

(defun set-piece-at! (state square piece)
  (update-bitboard state piece (ash 1 square) logior)
  (setf (aref (mailbox state) square) piece))

