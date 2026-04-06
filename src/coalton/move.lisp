(in-package :qiku.coalton)

(defun do-move! (position move)
  (qiku:do-move! (coalton-position-cl-position position) move))

(defun undo-move! (position move)
  (qiku:undo-move! (coalton-position-cl-position position) move))
