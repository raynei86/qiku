(in-package :qiku)

(deftype piece-color () '(member :black :white))
(deftype piece-type () '(member :pawn :rook :knight :bishop :queen :king :empty))

(defstruct piece
  (type :empty)
  (color nil))

;; More commonly known as "position", but that's a reserved name
(defclass state ()
  ((mailbox
    :initarg :mailbox
    :accessor mailbox
    :initform (generate-piece
	       '(br bn bb bq bk bb bn br
		 bp bp bp bp bp bp bp bp
		 00 00 00 00 00 00 00 00
		 00 00 00 00 00 00 00 00
		 00 00 00 00 00 00 00 00
		 00 00 00 00 00 00 00 00
		 wp wp wp wp wp wp wp wp
		 wr wn wb wq wk wb wn wr))
    :type (simple-vector 64)
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
    :type piece-color
    :documentation "The side that is moving")
   (castling-rights
    :initarg :castling-rights
    :accessor castling-rights
    :type (unsigned-byte 4)
    :documentation "Castling rights represented as a bitmask (white is first): KQkq")
   (ep-square :initarg :ep-square
	      :accessor ep-square
	      :type (or null (integer 0 63)))
   (halfmove-clock :initarg :halfmove-clock
		   :accessor halfmove-clock
		   :type (integer))
   (fullmove-number :initarg :fullmove-number
		    :accessor fullmove-number
		    :type (integer 1))))

(defmethod white-pieces ((state state))
  (logior (white-pawns state) (white-knights state) (white-bishops state)
          (white-rooks state) (white-queens state) (white-king state)))

(defmethod black-pieces ((state state))
  (logior (black-pawns state) (black-knights state) (black-bishops state)
          (black-rooks state) (black-queens state) (black-king state)))

(defmethod all-pieces ((state state))
  (logior (white-pieces state) (black-pieces state)))

;; Terribly ugly...
(defun clear-piece-at (state square piece)
  (let ((bit (lognot (ash 1 square))))
    (case (piece-color piece)
      (:white (case (piece-type piece)
		(:pawn (setf (white-pawns state) (logand (white-pawns state) bit)))
		(:rook (setf (white-rooks state) (logand (white-rooks state) bit)))
		(:knight (setf (white-knights state) (logand (white-knights state) bit)))
		(:bishop (setf (white-bishops state) (logand (white-bishops state) bit)))
		(:queen (setf (white-queens state) (logand (white-queens state) bit)))
		(:king (setf (white-king state) (logand (white-king state) bit)))))
      (:black (case (piece-type piece)
		(:pawn (setf (black-pawns state) (logand (black-pawns state) bit)))
		(:rook (setf (black-rooks state) (logand (black-rooks state) bit)))
		(:knight (setf (black-knights state) (logand (black-knights state) bit)))
		(:bishop (setf (black-bishops state) (logand (black-bishops state) bit)))
		(:queen (setf (black-queens state) (logand (black-queens state) bit)))
		(:king (setf (black-king state) (logand (black-king state) bit)))))
      (otherwise nil)))
  (setf (piece-type (aref (mailbox state) square)) :empty))


(defun set-piece-at (state square piece)
  (let ((bit (ash 1 square)))
    (case (piece-color piece)
      (:white (case (piece-type piece)
		(:pawn (setf (white-pawns state) (logand (white-pawns state) bit)))
		(:rook (setf (white-rooks state) (logand (white-rooks state) bit)))
		(:knight (setf (white-knights state) (logand (white-knights state) bit)))
		(:bishop (setf (white-bishops state) (logand (white-bishops state) bit)))
		(:queen (setf (white-queens state) (logand (white-queens state) bit)))
		(:king (setf (white-king state) (logand (white-king state) bit)))))
      (:black (case (piece-type piece)
		(:pawn (setf (black-pawns state) (logand (black-pawns state) bit)))
		(:rook (setf (black-rooks state) (logand (black-rooks state) bit)))
		(:knight (setf (black-knights state) (logand (black-knights state) bit)))
		(:bishop (setf (black-bishops state) (logand (black-bishops state) bit)))
		(:queen (setf (black-queens state) (logand (black-queens state) bit)))
		(:king (setf (black-king state) (logand (black-king state) bit)))))
      (otherwise nil)))
  (setf (aref (mailbox state) square) piece))

