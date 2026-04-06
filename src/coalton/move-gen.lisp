(in-package :qiku.coalton)

(defun generate-pseudolegal-moves (position)
  (qiku:generate-pseudolegal-moves (coalton-position-cl-position position)))

(defun generate-legal-moves (position)
  (qiku:generate-legal-moves (coalton-position-cl-position position)))
