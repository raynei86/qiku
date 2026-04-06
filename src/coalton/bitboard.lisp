(in-package :qiku.coalton)

(defun bitboard-squares (bitboard)
  "Temporary wrapper to keep bitboard helpers reachable from the migration layer."
  (qiku:bb-squares bitboard))
