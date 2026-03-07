(defpackage :qiku
  (:use :cl :iterate)
  (:export
   ;; Pieces
   #:make-piece
   #:generate-piece
   #:piece-color
   #:piece-type
   #:+white+
   #:+black+
   #:+empty+
   #:+pawn+
   #:+rook+
   #:+knight+
   #:+bishop+
   #:+queen+
   #:+king+

   ;; Position
   #:state
   #:mailbox
   #:white-pawns
   #:white-knights
   #:white-bishops
   #:white-rooks
   #:white-queens
   #:white-king
   #:black-pawns
   #:black-knights
   #:black-bishops
   #:black-rooks
   #:black-queens
   #:black-king
   #:white-pieces
   #:black-pieces
   #:all-pieces
   #:set-piece-at
   #:clear-piece-at

   ;; Moves
   #:move
   #:do-move
   #:+capture-flag+
   #:+en-passant-flag+
   #:+castling-flag+
   #:+double-pawn-push-flag+
   #:+promotion-flag+
   #:knight-moves
   #:bishop-moves
   #:rook-moves
   #:queen-moves
   #:pawn-moves
   #:king-moves
   #:king-in-check-p
   #:square-attacked-p
   #:generate-pseudolegal-moves
   #:generate-legal-moves
   
   ;; Utils
   #:square-algebraic
   #:king-square
   #:square-rank
   #:square-file
   #:square-occupied-p
   #:square-occupied-by-p
   #:checkmate-p
   #:stalemate-p))

(in-package #:qiku)

