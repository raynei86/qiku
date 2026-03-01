(defpackage qiku/tests/main
  (:use :cl
        :qiku
        :rove))
(in-package :qiku/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :qiku)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
