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

(defun do-move! (position move)
  (let* ((from (move-from move))
         (to (move-to move))
         (piece (move-piece move))
         (captured (move-captured move))
         (promotion (move-promotion move))
         (flags (move-flags move))
	 (color (piece-color piece)))

    (setf (move-old-ep-square       move) (position-ep-square       position)
          (move-old-castling-rights move) (position-castling-rights position)
          (move-old-halfmove-clock  move) (position-halfmove-clock  position))
    
    ;; Clocks
    (when (or (eql (piece-type piece) +pawn+) captured)
      (setf (position-halfmove-clock position) 0))
    	(incf (position-halfmove-clock position))
    (when (eql color +black+)
      (incf (position-fullmove-number position)))

    ;; Turn
    (setf (position-turn position) (enemy-of color))

    ;; Ep square
    (setf (position-ep-square position)
          (when (logtest flags +double-pawn-push-flag+)
            (if (eql color +white+) (+ from 8) (- from 8))))

    ;; Castling
    (setf (position-castling-rights position)
          (compute-castling-rights
           (position-castling-rights position) from to
           (piece-type piece) color
           (and captured (piece-type captured))))

    ;; Mutations
    (clear-piece-at! position from)

    (cond
      ((logtest flags +en-passant-flag+)
       (clear-piece-at! position (if (eql color +white+) (- to 8) (+ to 8))))
      (captured
       (clear-piece-at! position to)))

    ;; Handle castling
    (when (logtest flags +castling-flag+)
      (let* ((rook-from (castling-rook-from from to))
             (rook-to   (castling-rook-to   from to))
             (rook      (piece-at position rook-from)))
        (clear-piece-at! position rook-from)
        (set-piece-at!   position rook-to   rook)))

    ;; Finally set the piece down
    (set-piece-at! position to (or promotion piece))

    position))

(defun undo-move! (position move)
  "Reverse the effect of a previous DO-MOVE! on POSITION."
  (let* ((from      (move-from      move))
         (to        (move-to        move))
         (piece     (move-piece     move))
         (captured  (move-captured  move))
         (flags     (move-flags     move))
         (color     (piece-color piece)))

    (setf (position-ep-square       position) (move-old-ep-square       move)
          (position-castling-rights position) (move-old-castling-rights move)
          (position-halfmove-clock  position) (move-old-halfmove-clock  move)
          (position-turn            position) color)
    (when (= color +black+)
      (decf (position-fullmove-number position)))

    (clear-piece-at! position to)

    (set-piece-at! position from piece)

    (cond
      ((logtest flags +en-passant-flag+)
       (set-piece-at! position (if (= color +white+) (- to 8) (+ to 8)) captured))
      (captured
       (set-piece-at! position to captured)))

    (when (logtest flags +castling-flag+)
      (let* ((rook-from (castling-rook-from from to))
             (rook-to   (castling-rook-to   from to))
             (rook      (piece-at position rook-to)))
        (clear-piece-at! position rook-to)
        (set-piece-at!   position rook-from rook)))

    position))


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
