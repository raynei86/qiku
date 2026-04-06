(in-package :qiku.coalton)

(defun perft (position depth)
  (qiku:perft (coalton-position-cl-position position) depth))
