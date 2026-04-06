(defpackage qiku/tests/main
  (:use :cl
        :qiku
        :rove))
(in-package :qiku/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :qiku)' in your Lisp.

(defun copy-position-snapshot (position)
  (list
   (copy-seq (position-mailbox position))
   (position-white-pawns position)
   (position-white-knights position)
   (position-white-bishops position)
   (position-white-rooks position)
   (position-white-queens position)
   (position-white-king position)
   (position-black-pawns position)
   (position-black-knights position)
   (position-black-bishops position)
   (position-black-rooks position)
   (position-black-queens position)
   (position-black-king position)
   (position-turn position)
   (position-castling-rights position)
   (position-ep-square position)
   (position-halfmove-clock position)
   (position-fullmove-number position)))

(defun position-snapshot= (left right)
  ;; The mailbox is a vector, so use EQUALP for element-wise array comparison.
  ;; Scalar metadata fields compare correctly with EQUAL.
  (and (equalp (first left) (first right))
       (equal (rest left) (rest right))))

(deftest perft-starting-position-smoke
  (testing "standard perft values for initial position"
    (let ((position (make-position)))
      (ok (= 20 (perft position 1)))
      (ok (= 400 (perft position 2)))
      (ok (= 8902 (perft position 3))))))

(deftest make-undo-roundtrip
  (testing "do-move! then undo-move! restores exact position snapshot"
    (let* ((position (make-position))
           (moves (generate-legal-moves position)))
      (dolist (move moves)
        (let ((before (copy-position-snapshot position)))
          (do-move! position move)
          (undo-move! position move)
          (ok (position-snapshot= before (copy-position-snapshot position))))))))
