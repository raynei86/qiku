(in-package :qiku)

(defstruct move
  (from 0 :type (integer 0 63))
  (to 0 :type (integer 0 63))
  (piece nil :type piece)
  (captured nil :type (or null piece))
  (promotion nil :type (or null piece))
  (flags 0 :type (unsigned-byte 5))
  (old-ep-square nil :type (or null (integer 0 63)))
  (old-castling-rights #b00000 :type (unsigned-byte 4))
  (old-halfmove-clock 0 :type (integer 0)))

(defun do-move! (state move)
  (let* ((from (move-from move))
         (to (move-to move))
         (piece (move-piece move))
         (captured (move-captured move))
         (promotion (move-promotion move))
         (flags (move-flags move))
	 (color (piece-color piece)))

    (setf (move-old-ep-square       move) (state-ep-square       state)
          (move-old-castling-rights move) (state-castling-rights state)
          (move-old-halfmove-clock  move) (state-halfmove-clock  state))
    
    ;; Clocks
    (when (or (eql (piece-type piece) +pawn+) captured)
      (setf (state-halfmove-clock state) 0))
    	(incf (state-halfmove-clock state))
    (when (eql color +black+)
      (incf (state-fullmove-number state)))

    ;; Turn
    (setf (state-turn state) (enemy-of color))

    ;; Ep square
    (setf (state-ep-square state)
          (when (logtest flags +double-pawn-push-flag+)
            (if (eql color +white+) (+ from 8) (- from 8))))

    ;; Castling
    (setf (state-castling-rights state)
          (compute-castling-rights
           (state-castling-rights state) from to
           (piece-type piece) color
           (and captured (piece-type captured))))

    ;; Mutations
    (clear-piece-at! state from)

    (cond
      ((logtest flags +en-passant-flag+)
       (clear-piece-at! state (if (eql color +white+) (- to 8) (+ to 8))))
      (captured
       (clear-piece-at! state to)))

    ;; Handle castling
    (when (logtest flags +castling-flag+)
      (let* ((rook-from (castling-rook-from from to))
             (rook-to   (castling-rook-to   from to))
             (rook      (piece-at state rook-from)))
        (clear-piece-at! state rook-from)
        (set-piece-at!   state rook-to   rook)))

    ;; Finally set the piece down
    (set-piece-at! state to (or promotion piece))

    state))

(defun undo-move! (state move)
  "Reverse the effect of a previous DO-MOVE! on STATE."
  (let* ((from      (move-from      move))
         (to        (move-to        move))
         (piece     (move-piece     move))
         (captured  (move-captured  move))
         (flags     (move-flags     move))
         (color     (piece-color piece)))

    (setf (state-ep-square       state) (move-old-ep-square       move)
          (state-castling-rights state) (move-old-castling-rights move)
          (state-halfmove-clock  state) (move-old-halfmove-clock  move)
          (state-turn            state) color)
    (when (= color +black+)
      (decf (state-fullmove-number state)))

    (clear-piece-at! state to)

    (set-piece-at! state from piece)

    (cond
      ((logtest flags +en-passant-flag+)
       (set-piece-at! state (if (= color +white+) (- to 8) (+ to 8)) captured))
      (captured
       (set-piece-at! state to captured)))

    (when (logtest flags +castling-flag+)
      (let* ((rook-from (castling-rook-from from to))
             (rook-to   (castling-rook-to   from to))
             (rook      (piece-at state rook-to)))
        (clear-piece-at! state rook-to)
        (set-piece-at!   state rook-from rook)))

    state))


(declaim (ftype (function (mailbox-index mailbox-index) mailbox-index) castling-rook-from) (inline castling-rook-from))
(defun castling-rook-from (king-from king-to)
  (if (> king-to king-from)
      (if (= king-from 4) 7 63)		; h1 or h8
      (if (= king-from 4) 0 56))) ; a1 or a8

(declaim (ftype (function (mailbox-index mailbox-index) mailbox-index) castling-rook-to)   (inline castling-rook-to))
(defun castling-rook-to (king-from king-to)
  (if (> king-to king-from)
      (if (= king-from 4) 5 61)		; f1 or f8
      (if (= king-from 4) 3 59))) ; d1 or d8

(declaim (ftype (function ((unsigned-byte 4) mailbox-index mailbox-index piece color (or null piece))
                          (unsigned-byte 4))
                compute-castling-rights))
(defun compute-castling-rights (rights from to piece-type piece-color captured-type)
  "Return updated castling rights after a move; does not mutate anything."
  (let ((r rights))
    ;; King move strips both rights for that side
    (when (eql piece-type +king+)
      (setf r (logand r (if (eql piece-color +white+) #b0011 #b1100))))
    ;; Rook leaving its home square strips one right
    (when (eql piece-type +rook+)
      (cond ((= from 0)  (setf r (logand r #b1011))) ; white queenside
            ((= from 7)  (setf r (logand r #b0111))) ; white kingside
            ((= from 56) (setf r (logand r #b1110))) ; black queenside
            ((= from 63) (setf r (logand r #b1101))))) ; black kingside
    ;; Rook captured on its home square strips one right
    (when (eql captured-type +rook+)
      (cond ((= to 0)  (setf r (logand r #b1011)))
            ((= to 7)  (setf r (logand r #b0111)))
            ((= to 56) (setf r (logand r #b1110)))
            ((= to 63) (setf r (logand r #b1101)))))
    r))
