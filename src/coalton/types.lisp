(in-package :qiku.coalton)

(defstruct (coalton-position
            (:constructor %make-position (&key (cl-position (qiku:make-position)))))
  (cl-position (qiku:make-position) :type qiku:position))

(defun make-position ()
  "Coalton migration wrapper for a qiku:position value."
  (%make-position :cl-position (qiku:make-position)))

(defun position-p (value)
  (typep value 'coalton-position))

(defun position-turn (position)
  (qiku:position-turn (coalton-position-cl-position position)))

(defun position-castling-rights (position)
  (qiku:position-castling-rights (coalton-position-cl-position position)))

(defun position-ep-square (position)
  (qiku:position-ep-square (coalton-position-cl-position position)))

(defun position-halfmove-clock (position)
  (qiku:position-halfmove-clock (coalton-position-cl-position position)))

(defun position-fullmove-number (position)
  (qiku:position-fullmove-number (coalton-position-cl-position position)))
