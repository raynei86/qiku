(defsystem "qiku"
  :version "0.0.1"
  :author "Lihui Zhang"
  :mailto "zlihui486@gmail.com"
  :license "LGPL-3"
  :depends-on ("iterate" "serapeum")
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "core" :depends-on ("package"))
		 (:file "utils" :depends-on ("core" "package" "move"))
		 (:file "move" :depends-on ("core" "package"))
		 (:file "move-gen" :depends-on ("core" "move" "package" "utils")))))
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
