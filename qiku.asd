(defsystem "qiku"
  :version "0.0.1"
  :author "Lihui Zhang"
  :mailto "zlihui486@gmail.com"
  :license "LGPL-3"
  :depends-on ("iterate" "serapeum" "str")
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "core" :depends-on ("package" "constants"))
		 (:file "utils" :depends-on ("core" "package" "move"))
		 (:file "constants")
		 (:file "move" :depends-on ("core" "package" "constants"))
		 (:file "move-gen" :depends-on ("core" "move" "package" "utils" "constants"))
		 (:file "uci" :depends-on ("core" "move" "constants")))))
  :description "A chess library in Common Lisp"
  :in-order-to ((test-op (test-op "qiku/tests"))))

(defsystem "qiku/tests"
  :author "Lihui Zhang"
  :license "LGPL-3"
  :depends-on ("qiku"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for qiku"
  :perform (test-op (op c) (symbol-call :rove :run c)))
