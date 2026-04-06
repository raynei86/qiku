(in-package :qiku.coalton)

(defun king-in-check-p (position color)
  (qiku:king-in-check-p (coalton-position-cl-position position) color))

(defun square-attacked-p (position square defender-color)
  (qiku:square-attacked-p (coalton-position-cl-position position) square defender-color))

(defun checkmate-p (position &optional (moves t))
  (qiku:checkmate-p (coalton-position-cl-position position) moves))

(defun stalemate-p (position &optional (moves t))
  (qiku:stalemate-p (coalton-position-cl-position position) moves))
