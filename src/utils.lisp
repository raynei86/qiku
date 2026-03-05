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
             :promotion (make-piece :type promo-type :color color)
             :flags (logior +promotion-flag+
                            (if captured +capture-flag+ 0))))
