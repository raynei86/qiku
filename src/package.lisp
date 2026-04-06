(defpackage :qiku
  (:use :cl :iterate)
  (:shadow #:position)
  (:export
   ;; Pieces
   #:make-piece
   #:make-position
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
   #:position
   #:position-mailbox
   #:position-white-pawns
   #:position-white-knights
   #:position-white-bishops
   #:position-white-rooks
   #:position-white-queens
   #:position-white-king
   #:position-black-pawns
   #:position-black-knights
   #:position-black-bishops
   #:position-black-rooks
   #:position-black-queens
   #:position-black-king
   #:position-turn
   #:position-castling-rights
   #:position-ep-square
   #:position-halfmove-clock
   #:position-fullmove-number
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
   #:perft

   ;; UCI
   #:uci-engine
   #:engine-name
   #:engine-author
   #:engine-position
   #:engine-options
   #:search-best-move
   #:on-new-game
   #:on-position
   #:on-quit
   #:uci-send
   #:uci-info
   #:uci-loop
   
   ;; Utils
   #:bb-squares
   #:square->algebraic
   #:king-square
   #:square-rank
   #:square-file
   #:square-occupied-p
   #:square-occupied-by-p
   #:checkmate-p
   #:stalemate-p
   #:+file-masks+
   #:+rank-masks+
   #:adjacent-files-mask
   #:adjacent-ranks-mask
   #:distance))

(in-package #:qiku)
