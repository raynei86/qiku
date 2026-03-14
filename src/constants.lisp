(in-package :qiku)

;; Pieces
(serapeum:defconst +white+ 0)
(serapeum:defconst +black+ 8)
(serapeum:defconst +empty+ 0)
(serapeum:defconst +pawn+  1)
(serapeum:defconst +rook+  2)
(serapeum:defconst +knight+ 3)
(serapeum:defconst +bishop+ 4)
(serapeum:defconst +queen+ 5)
(serapeum:defconst +king+  6)

;; Move offsets
(serapeum:defconst +knight-offsets+ '(+17 +15 +10 +6 -6 -10 -15 -17))
(serapeum:defconst +king-offsets+ '(+9 +8 +7 +1 -1 -7 -8 -9))
(serapeum:defconst +rook-directions+   '((1 . 0) (-1 . 0) (0 . 1) (0 . -1)))
(serapeum:defconst +bishop-directions+ '((1 . 1) (1 . -1) (-1 . 1) (-1 . -1)))
(serapeum:defconst +queen-directions+  (append +rook-directions+ +bishop-directions+))

;; Precomputed attack boards
(serapeum:defconst +knight-attacks+
  (iterate
    (with table = (make-array 64 :element-type '(unsigned-byte 64) :initial-element 0))
    (for square to 63)
    (setf (aref table square)
	  (iterate
	    (for offset in +knight-offsets+)
	    (for to = (+ square offset))
	    (when (and (<= 0 to 63)
		       (<= (abs (- (logand to 7) (logand square 7))) 2))
	      (sum (ash 1 to)))))
    (finally (return table))))

(serapeum:defconst +king-attacks+
  (iterate
    (with table = (make-array 64 :element-type '(unsigned-byte 64) :initial-element 0))
    (for square to 63)
    (setf (aref table square)
	  (iterate
	    (for offset in +king-offsets+)
	    (for to = (+ square offset))
	    (when (and (<= 0 to 63)
		       (<= (abs (- (logand to 7) (logand square 7))) 1))
	      (sum (ash 1 to)))))
    (finally (return table))))

(serapeum:defconst +pawn-attacks+
  (iterate
    (with table = (make-array '(2 64) :element-type '(unsigned-byte 64) :initial-element 0))
    (for square to 63)
    (setf (aref table 0 square)
	  (iterate
	    (for offset in '(7 9))
	    (for to = (+ square offset))
	    (when (and (< square 56)
		       (<= (abs (- (logand to 7) (logand square 7))) 1))
	      (sum (ash 1 to)))))
    (setf (aref table 1 square)
	  (iterate
	    (for offset in '(-7 -9))
	    (for to = (+ square offset))
	    (when (and (< square 56)
		       (<= (abs (- (logand to 7) (logand square 7))) 1))
	      (sum (ash 1 to)))))
    (finally (return table))))

;; Move related flags
(serapeum:defconst +capture-flag+        #b00001)
(serapeum:defconst +en-passant-flag+     #b00010)
(serapeum:defconst +castling-flag+       #b00100)
(serapeum:defconst +double-pawn-push-flag+ #b01000)
(serapeum:defconst +promotion-flag+      #b10000)

;; General masks for utility
(serapeum:defconst +file-masks+
  (iter
    (with masks = (make-array 8 :element-type '(unsigned-byte 64)))
    (for file from 0 to 7)
    (setf (aref masks file)
	  (iter
	    (for rank from 0 to 7)
	    (sum (ash 1 (+ (* rank 8) file)))))
    (finally (return masks))))

(serapeum:defconst +rank-masks+
  (iter
    (with masks = (make-array 8 :element-type '(unsigned-byte 64)))
    (for rank from 0 to 7)
    (setf (aref masks rank)
	  (iter
	    (for file from 0 to 7)
	    (sum (ash 1 (+ (* rank 8) file)))))
    (finally (return masks))))

