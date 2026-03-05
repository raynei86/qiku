;;; Big file of crap so I can keep the implementation cleaner
(in-package :qiku)

(defun generate-piece (piece-list)
  (flet ((to-piece (p)
	   (case p
	     (bp (make-piece :type :pawn :color :black))
	     (wp (make-piece :type :pawn :color :white))
	     (br (make-piece :type :rook :color :black))
	     (wr (make-piece :type :rook :color :white))
	     (bn (make-piece :type :knight :color :black))
	     (wn (make-piece :type :knight :color :white))
	     (bb (make-piece :type :bishop :color :black))
	     (wb (make-piece :type :bishop :color :white))
	     (bq (make-piece :type :queen :color :black))
	     (wq (make-piece :type :queen :color :white))
	     (bk (make-piece :type :king :color :black))
	     (wk (make-piece :type :king :color :white))
	     (otherwise (make-piece :type :empty :color nil)))))
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
