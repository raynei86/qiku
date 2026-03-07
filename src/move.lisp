(in-package :qiku)

(defconstant +capture-flag+        #b00001)
(defconstant +en-passant-flag+     #b00010)
(defconstant +castling-flag+       #b00100)
(defconstant +double-pawn-push-flag+ #b01000)
(defconstant +promotion-flag+      #b10000)

(defstruct move
  (from 0 :type (integer 0 63))
  (to 0 :type (integer 0 63))
  (piece nil :type integer)
  (captured nil :type (or null integer))
  (promotion nil :type (or null integer))
  (flags 0 :type (unsigned-byte 8)))

(defun do-move (state move)
  (let* ((s (copy-state state))
	 (from (move-from move))
         (to (move-to move))
         (piece (move-piece move))
         (captured (move-captured move))
         (promotion (move-promotion move))
         (flags (move-flags move))
	 (color (piece-color piece)))

    ;; Clocks
    (when (or (eql (piece-type piece) +pawn+) captured)
      (setf (halfmove-clock s) 0))
    	(incf (halfmove-clock s))
    (when (eql color +black+)
      (incf (fullmove-number s)))

    ;; Turn
    (setf (turn s) (if (eql color +white+) :black :white))

    ;; Ep square
    (setf (ep-square s)
          (when (logtest flags +double-pawn-push-flag+)
            (if (eql color +white+) (+ from 8) (- from 8))))

    ;; Castling
    (setf (castling-rights s)
          (compute-castling-rights
           (castling-rights s) from to
           (piece-type piece) color
           (and captured (piece-type captured))))

    ;; Mutations
    (clear-piece-at s from piece)

    (cond
      ((logtest flags +en-passant-flag+)
       (clear-piece-at s (if (eql color +white+) (- to 8) (+ to 8)) captured))
      (captured
       (clear-piece-at s to captured)))

    ;; Handle castling
    (when (logtest flags +castling-flag+)
      (let* ((rook-from (castling-rook-from from to))
             (rook-to   (castling-rook-to   from to))
             (rook      (piece-at s rook-from)))
        (clear-piece-at s rook-from rook)
        (set-piece-at   s rook-to   rook)))

    ;; Finally set the piece down
    (set-piece-at s to (or promotion piece))

    s))

(defun castling-rook-from (king-from king-to)
  (if (> king-to king-from)
      (if (= king-from 4) 7 63)		; h1 or h8
      (if (= king-from 4) 0 56))) ; a1 or a8

(defun castling-rook-to (king-from king-to)
  (if (> king-to king-from)
      (if (= king-from 4) 5 61)   ; f1 or f8
      (if (= king-from 4) 3 59))) ; d1 or d8

(defun compute-castling-rights (rights from to piece-type piece-color captured-type)
  "Return updated castling rights after a move; does not mutate anything."
  (let ((r rights))
    ;; King move strips both rights for that side
    (when (eql piece-type +king+)
      (setf r (logand r (if (eql piece-color :white) #b0011 #b1100))))
    ;; Rook leaving its home square strips one right
    (when (eql piece-type +rook+)
      (cond ((= from 0)  (setf r (logand r #b1011)))   ; white queenside
            ((= from 7)  (setf r (logand r #b0111)))   ; white kingside
            ((= from 56) (setf r (logand r #b1110)))   ; black queenside
            ((= from 63) (setf r (logand r #b1101))))) ; black kingside
    ;; Rook captured on its home square strips one right
    (when (eql captured-type +rook+)
      (cond ((= to 0)  (setf r (logand r #b1011)))
            ((= to 7)  (setf r (logand r #b0111)))
            ((= to 56) (setf r (logand r #b1110)))
            ((= to 63) (setf r (logand r #b1101)))))
    r))
