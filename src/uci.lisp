(in-package :qiku)

(defclass uci-engine ()
  ((name
    :initarg :name
    :reader engine-name
    :initform "Unnamed Engine"
    :type string)
   (author
    :initarg :author
    :reader engine-author
    :initform "Unknown"
    :type string)
   (state
    :accessor engine-state
    :initform (make-state)
    :type state)
   (options
    :accessor engine-options
    :initform '()
    :type list)))

(defgeneric search-best-move (engine state depth)
  (:documentation "Return the best move for current position at depth. Must be specialized."))

(defgeneric on-new-game (engine)
  (:documentation "Called on `ucinewgame`. Reset any engine-specific state.")
  (:method ((engine uci-engine))
    ;; Currently just creating a new `state` object is enough
    (setf (engine-state engine) (make-state))))

(defgeneric on-position (engine state)
  (:documentation "Called after a position has been set up and all moves applied.")
  (:method ((engine uci-engine) state)
    (declare (ignore state))
    nil))

(defgeneric on-quit (engine)
  (:documentation "Called on `quit`.")
  (:method ((engine uci-engine)) nil))


(defun uci-send (fmt &rest args)
  "Send a line to the GUI"
  (apply #'format t fmt args)
  (terpri))

(defun uci-info (fmt &rest args)
  "Send an 'info string ...' to GUI"
  (apply #'uci-send (str:concat "info string " fmt) args))



(defun find-uci-move (state token)
  "Match a UCI move string against legal moves"
  (find-if (lambda (move)
	     (let ((base (format nil "~a~a"
				 (square->algebraic (move-from move))
				 (square->algebraic (move-to move)))))
	       (if (logtest (move-flags move) +promotion-flag+)
		   (and (= (length token) 5)
			(string= base (str:substring 0 4 token))
			(member (aref token 4) '(#\q #\r #\b #\n) :test #'char=))
		   (string= base token))))
	   (generate-legal-moves state)))

(defun apply-moves (state tokens)
  "Apply a sequence of UCI moves to state"
  (iter
    (for token in tokens)
    (for move = (find-uci-move state token))
    (when move (do-move! state move))))

(defun handle-go (engine tokens)
  "Call `search-best-move` based on depth from tokens and print out best move. Default depth is 15."
  (let* ((depth-pos (position "depth" tokens :test #'string=))
	 (depth (if depth-pos
		    (parse-integer (nth (1+ depth-pos) tokens))
		    15)))
    (let ((move (search-best-move engine (engine-state engine) depth)))
      (if move
	  (uci-send "bestmove ~a~a~@[~a~]"
		    (square->algebraic (move-from move))
		    (square->algebraic (move-to move))
		    (when (logtest (move-flags move) +promotion-flag+)
		      (serapeum:tree-ecase (piece-type (move-promotion move))
			(#.+queen+ "q")
			(#.+rook+ "r")
			(#.+bishop+ "b")
			(#.+knight+ "n"))))
	  (uci-send "bestmove 0000")))))

(defun handle-position (engine tokens)
  "Handle position by resetting state and replaying moves."
  (setf (engine-state engine) (make-state))
  (let ((moves-pos (position "moves" tokens :test #'string=)))
    (when moves-pos
      (apply-moves (engine-state engine)
                   (subseq tokens (1+ moves-pos)))))
  (on-position engine (engine-state engine)))

(defun uci-loop (engine)
  "Main UCI loop. Blocks until `quit` is received"
  ;; Handshake
  (uci-send "id name ~a"   (engine-name   engine))
  (uci-send "id author ~a" (engine-author engine))
  (uci-send "uciok")

  (iter
    (for line = (read-line *standard-input* nil nil))
    (while line)
    (for tokens = (str:split #\Space line))
    (for cmd = (first tokens))
    (for rest = (rest tokens))
    (serapeum:string-case cmd
      ("isready" (uci-send "readyok"))
      ("ucinewgame" (on-new-game engine))
      ("position" (handle-position engine rest))
      ("go" (handle-go engine rest))
      ("quit" (on-quit engine) (return)))))


