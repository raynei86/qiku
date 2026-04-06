(in-package :qiku.coalton)

(defun piece-at (position square)
  (qiku:piece-at (coalton-position-cl-position position) square))

(defun set-piece-at! (position square piece)
  (qiku:set-piece-at! (coalton-position-cl-position position) square piece))

(defun clear-piece-at! (position square)
  (qiku:clear-piece-at! (coalton-position-cl-position position) square))
