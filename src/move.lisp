(in-package :qiku)

(defconstant +capture-flag+        #b00001)
(defconstant +en-passant-flag+     #b00010)
(defconstant +castling-flag+       #b00100)
(defconstant +double-pawn-push-flag+ #b01000)
(defconstant +promotion-flag+      #b10000)

(defstruct move
  (from 0 :type (integer 0 63))
  (to 0 :type (integer 0 63))
  (piece nil :type piece)
  (captured nil :type (or null piece))
  (promotion nil :type (or null piece))
  (flags 0 :type (unsigned-byte 8))
  (old-castling-rights 0 :type (unsigned-byte 4))
  (old-ep-square nil :type (or null (integer 0 63)))
  (old-halfmove-clock 0 :type (integer 0))
  (old-fullmove-number 1 :type (integer 1)))


(defun update-castling-rights (state move)
  (let* ((from (move-from move))
         (to (move-to move))
         (type (piece-type (move-piece move)))
         (color (piece-color (move-piece move)))
         (rights (castling-rights state))
	 (captured (piece-type (move-captured move))))
    
    (when (eql type :king)
      (setf rights
            (logand rights
                    (if (eql color :white) #b0011 #b1100))))
    
    ;; Rook moves from original square → remove corresponding right
    (when (eql type :rook)
      (cond ((= from 0)  (setf rights (logand rights #b1011))) ; white queenside
            ((= from 7)  (setf rights (logand rights #b0111))) ; white kingside
            ((= from 56) (setf rights (logand rights #b1110))) ; black queenside
            ((= from 63) (setf rights (logand rights #b1101))))) ; black kingside
    
    ;; Rook captured on original square → remove corresponding right
    (when (eql captured :rook)
      (cond ((= to 0)  (setf rights (logand rights #b1011)))
	    ((= to 7)  (setf rights (logand rights #b0111)))
	    ((= to 56) (setf rights (logand rights #b1110)))
	    ((= to 63) (setf rights (logand rights #b1101)))))
    (setf (castling-rights state) rights)))

(defun do-move (state move)
  (let* ((from (move-from move))
         (to (move-to move))
         (piece (move-piece move))
         (captured (move-captured move))
         (promotion (move-promotion move))
         (flags (move-flags move))
         (old-castling (castling-rights state))
         (old-ep (ep-square state))
         (old-halfmove (halfmove-clock state))
         (old-fullmove (fullmove-number state)))
    ;; Store old state in the move for later undo
    (setf (move-old-castling-rights move) old-castling
          (move-old-ep-square move) old-ep
          (move-old-halfmove-clock move) old-halfmove
          (move-old-fullmove-number move) old-fullmove)
    
    (when (or (eql (piece-type piece) :pawn) captured)
	(setf (halfmove-clock state) 0)
	(incf (halfmove-clock state)))

    (when (eql (turn state) :black)
      (incf (fullmove-number state)))

    (setf (turn state) (if (eql (turn state) :white) :black :white))

    (setf (ep-square state) nil)

    (update-castling-rights state move)

    (clear-piece-at state from piece)

    ;; Handle capture
    (when (and captured (not (logtest flags +en-passant-flag+)))
      (clear-piece-at state to captured))

    (when (logtest flags +en-passant-flag+)
      (let ((ep-capture-square (if (eql (piece-color piece) :white) (- to 8) (+ to 8))))
	(clear-piece-at state ep-capture-square captured)))

    ;; Handle double pawn push
    (when (logtest flags +double-pawn-push-flag+)
      (setf (ep-square state)
	    (if (eql (piece-color piece) :white) (+ 8 from) (- 8 to))))

    ;; Handle castling
    (when (logtest flags +castling-flag+)
      (let ((rook-from (castling-rook-from from to))
	    (rook-to (castling-rook-to from to)))
	(clear-piece-at state rook-from piece)
	(set-piece-at state rook-to piece)))

    (let ((final-piece (if promotion promotion piece)))
      (set-piece-at state to final-piece))

    move))

(defun undo-move (state move)
  (setf (castling-rights state) (move-old-castling-rights move)
        (ep-square state) (move-old-ep-square move)
        (halfmove-clock state) (move-old-halfmove-clock move)
        (fullmove-number state) (move-old-fullmove-number move))

  (setf (turn state)
	(if (eql (turn state) :white) :black :white))

   (let ((from (move-from move))
        (to (move-to move))
        (piece (move-piece move))
        (captured (move-captured move))
        (promotion (move-promotion move))
        (flags (move-flags move)))

    (let ((dest-piece (if promotion promotion piece)))
      (clear-piece-at state to dest-piece))

    (when (and captured (not (logtest flags +en-passant-flag+)))
      (set-piece-at state to captured))

    (when (logtest flags +en-passant-flag+)
      (let ((ep-capture-square (if (eql (piece-color piece) :white) (- to 8) (+ to 8))))
        (set-piece-at state ep-capture-square captured)))

    (when (logtest flags +castling-flag+)
      (let ((rook-from (castling-rook-from from to))
            (rook-to (castling-rook-to from to))
            (rook-piece (if (eql (piece-color piece) :white) :white-rook :black-rook)))
        (clear-piece-at state rook-to rook-piece)
        (set-piece-at state rook-from rook-piece)))

    (set-piece-at state from piece)))

(defmacro with-move ((state move) &body body)
  (let ((move-var (gensym)))
    `(let ((,move-var ,move))
       (do-move ,state ,move-var)
       (unwind-protect
	    (progn ,@body)
	 (undo-move ,state ,move-var)))))

(defun castling-rook-from (king-from king-to)
  (if (> king-to king-from)
      (if (= king-from 4) 7 63)		; h1 or h8
      (if (= king-from 4) 0 56))) ; a1 or a8

(defun castling-rook-to (king-from king-to)
  (if (> king-to king-from)
      (if (= king-from 4) 5 61)   ; f1 or f8
      (if (= king-from 4) 3 59))) ; d1 or d8
