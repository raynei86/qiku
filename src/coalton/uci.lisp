(in-package :qiku.coalton)

(defclass coalton-uci-engine ()
  ((cl-engine
    :initarg :cl-engine
    :reader coalton-uci-engine-cl-engine
    :type qiku:uci-engine)))

(defun make-uci-engine (&key (name "Qiku") (author "Qiku") position)
  "Coalton migration wrapper over qiku:uci-engine."
  (let ((position (or position (make-position))))
  (make-instance 'coalton-uci-engine
                 :cl-engine (make-instance 'qiku:uci-engine
                                           :name name
                                           :author author
                                           :position (coalton-position-cl-position position)))))
