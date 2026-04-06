(in-package :qiku.coalton)

(defclass coalton-uci-engine ()
  ((cl-engine
    :initarg :cl-engine
    :reader coalton-uci-engine-cl-engine
    :type qiku:uci-engine)))

(defun make-uci-engine (&key (name "Qiku") (author "Qiku") (position (make-position)))
  "Coalton migration wrapper over qiku:uci-engine."
  (make-instance 'coalton-uci-engine
                 :cl-engine (make-instance 'qiku:uci-engine
                                           :name name
                                           :author author
                                           :position (coalton-position-cl-position position))))
