(defpackage :qiku
  (:use :cl :iterate)
  (:export
   ;; Pieces
   #:make-piece
   #:make-state
   #:generate-piece
   #:piece-color
   #:piece-type
   #:piece-at
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
   #:state-mailbox
   #:state-white-pawns
   #:state-white-knights
   #:state-white-bishops
   #:state-white-rooks
   #:state-white-queens
   #:state-white-king
   #:state-black-pawns
   #:state-black-knights
   #:state-black-bishops
   #:state-black-rooks
   #:state-black-queens
   #:state-black-king
   #:state-turn
   #:state-castling-rights
   #:state-ep-square
   #:state-halfmove-clock
   #:state-fullmove-number
   #:set-piece-at!
   #:clear-piece-at!

   ;; Moves
   #:move
   #:move-from
   #:move-to
   #:move-piece
   #:move-captured
   #:move-promotion
   #:move-flags
   #:do-move!
   #:undo-move!
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
   #:square->algebraic
   #:king-square
   #:square-rank
   #:square-file
   #:square-occupied-p
   #:square-occupied-by-p
   #:checkmate-p
   #:stalemate-p
   #:+file-mask+
   #:+rank-mask+
   #:adjacent-files-mask
   #:adjacent-ranks-mask))

(in-package #:qiku)
