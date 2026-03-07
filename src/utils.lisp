;;; Big file of crap so I can keep the implementation cleaner
(in-package :qiku)

(defun generate-piece (piece-list)
  (flet ((to-piece (p)
	   (case p
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
	     (otherwise (make-piece :white :empty)))))
    (coerce (mapcar #'to-piece piece-list) 'simple-vector)))

(defmacro update-bitboard (state piece bit op)
  `(case (piece-color piece)
     (#.+white+
      (case (piece-type piece)
	(:pawn   (setf (white-pawns ,state) (,op (white-pawns ,state) ,bit)))
        (:rook   (setf (white-rooks ,state) (,op (white-rooks ,state) ,bit)))
        (:knight (setf (white-knights ,state) (,op (white-knights ,state) ,bit)))
        (:bishop (setf (white-bishops ,state) (,op (white-bishops ,state) ,bit)))
        (:queen  (setf (white-queens ,state) (,op (white-queens ,state) ,bit)))
        (:king   (setf (white-king ,state) (,op (white-king ,state) ,bit)))))
     (#.+black+
      (case (piece-type ,piece)
        (:pawn   (setf (black-pawns ,state) (,op (black-pawns ,state) ,bit)))
        (:rook   (setf (black-rooks ,state) (,op (black-rooks ,state) ,bit)))
        (:knight (setf (black-knights ,state) (,op (black-knights ,state) ,bit)))
        (:bishop (setf (black-bishops ,state) (,op (black-bishops ,state) ,bit)))
        (:queen  (setf (black-queens ,state) (,op (black-queens ,state) ,bit)))
        (:king   (setf (black-king ,state) (,op (black-king ,state) ,bit)))))
     (otherwise nil)))

(defun copy-state (state)
  (make-instance 'state
    :mailbox         (copy-seq (mailbox state))
    :white-pawns     (white-pawns   state)
    :white-knights   (white-knights state)
    :white-bishops   (white-bishops state)
    :white-rooks     (white-rooks   state)
    :white-queens    (white-queens  state)
    :white-king      (white-king    state)
    :black-pawns     (black-pawns   state)
    :black-knights   (black-knights state)
    :black-bishops   (black-bishops state)
    :black-rooks     (black-rooks   state)
    :black-queens    (black-queens  state)
    :black-king      (black-king    state)
    :turn            (turn            state)
    :castling-rights (castling-rights state)
    :ep-square       (ep-square       state)
    :halfmove-clock  (halfmove-clock  state)
    :fullmove-number (fullmove-number state)))

(defun piece-at (state square)
  (aref (mailbox state) square))

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
