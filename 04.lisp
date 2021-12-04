;;; Advent of Code 2021, Day 4
;;; J Jensen
;;;
;;; Entrypoints: functions #'solve1 and #'solve2 below


(defconstant +day4-test-input+
"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
")



(defun read-row (line)
  "Read a row of integers (in a string), returning a list of the numbers"
  (let ((seq (split-sequence:split-sequence (if (find #\, line) #\, #\Space) line :remove-empty-subseqs t)))
    (map-into seq #'parse-integer seq)))


#+rt
(rt:deftest (4 read-row 1)
    (read-row "1,2,3,4,5,6,7,8")
  (1 2 3 4 5 6 7 8))

#+rt
(rt:deftest (4 read-row 2)
    (read-row "")
  nil)

#+rt
(rt:deftest (4 read-row 3)
    (read-row "  12  34    67  94")
  (12 34 67 94))



(defclass board nil
  ((nrow :initarg :nrow :initform 5 :accessor board-nrow :documentation "Number of rows")
   (ncol :initarg :ncol :initform 5 :accessor board-ncol :documentation "Number of columns")
   (elts :initarg :elts :type array :accessor board-elts :documentation "Board elements")
   (mark :initarg :mark :type array :accessor board-mark :documentation "Boolean whether slot is selected"))
  (:documentation "Board with numbers that can be marked as selected"))



(defmethod extract-into ((b board) (v vector) row col)
  "Extract a row or column from board-mark into a (simple) vector v"
  (unless (or (null row) (null col)) (error "One of row or col should be NIL"))
  (if row (loop for c from 0 below (board-ncol b) do (setf (svref v c) (aref (board-mark b) row c)))
      (loop for r from 0 below (board-nrow b) do (setf (svref v r) (aref (board-mark b) r col))))
  v)



(defmethod win ((b board))
  "check whether a board has won"
  (with-slots (nrow ncol mark) b
    ;; Check rows for completion
    (loop for r from 0 below nrow
	  with v = (make-array ncol)
	  do (extract-into b v r nil)
	  when (every #'identity v) do (return-from win t))
    ;; Same for columns
    (loop for c from 0 below ncol
	  with v = (make-array nrow)
	  do (extract-into b v nil c)
	  when (every #'identity v) do (return-from win t)))
  nil)



(defmethod play ((b board) (k integer))
  "Mark all occurrences of k in board"
  ;; Please excuse the pedestrian implementation
  (with-slots (nrow ncol elts mark) b
    (loop for c from 0 below ncol
	  do (loop for r from 0 below nrow
		   when (eql k (aref elts r c))
		     do (setf (aref mark r c) t)))))



(defmethod score ((b board))
  "Sum of unmarked numbers"
  ;; Slightly more exciting implementation
  (let* ((size (* (board-nrow b) (board-ncol b)))
	 (e (make-array size :displaced-to (board-elts b)))
	 (m (make-array size :displaced-to (board-mark b))))
    (reduce #'+
	    (apply #'nconc
		   (map 'list (lambda (n d) (if d nil (list n))) e m)))))



(defun make-board (s)
  "Make a board reading from a stream"
  (let* ((data (loop for line = (read-line s nil)
		     until (or (null line) (equal line ""))
		     collect (read-row line)))
	 (data-rows (length data))
	 (data-cols (prog1 (length (first data)) (when (and data (not (apply #'= (mapcar #'length data)))) (error "rows of unequal length ~S" data)))))
    (if data
	(make-instance 'board
		       :nrow data-rows
		       :ncol data-cols
		       :elts (make-array (list data-rows data-cols) :initial-contents data)
		       :mark (make-array (list data-rows data-cols) :initial-element nil))
	nil)))



(defun read-data (s)
  (let ((seq (read-row (read-line s nil)))
	(empty (read-line s nil))		; discard following empty line
	(boards (loop for b = (make-board s)
		      while b
		      collect b)))
    (unless (equal "" empty) (error "Expected second line to be empty"))
    (values seq boards)))



(defun play-game1 (seq boards)
  "Play the sequence of integers until a winning board appears, returning the last number played and the winning board"
  (unless seq (return-from play-game1 nil)) ; no one wins
  (let ((k (first seq))			   ; element to play
	(winners nil))
    (map nil (lambda (b) (play b k) (when (win b) (push b winners))) boards)
    (case (length winners)
      (1 (return-from play-game1 (values k (first winners))))
      (0 (play-game (rest seq) boards))
      (t (error "More than one winner found")))))



(defun solve-stream (s play-function)
  "Read data and play the game until a winning board appears, returning the last value played and the board"
  (multiple-value-bind (seq boards) (read-data s)
    (multiple-value-bind (num winner) (funcall play-function seq boards)
      (when num (* num (score winner))))))


(defun solve (input play-function)
  (etypecase input
    (stream (solve-stream input play-function))
    (pathname (with-open-file (foo input :direction :input :if-does-not-exist :error) (solve-stream foo play-function)))
    (string (solve-stream (make-string-input-stream input) play-function))))


(defun solve1 (input)
  (solve input #'play-game1))


(defun play-game2 (seq boards)
  "Play the game until all boards have won, returning the number played and the last board that wins. May modify the boards sequence."

  (when (or (endp seq) (endp boards))
    (error "Shouldn't happen: ~D numbers and ~D boards left" (length seq) (length boards)))

  (let ((k (first seq)))
    ;; Play k across all boards
    (map nil (lambda (b) (play b k)) boards)

    ;; When we have one element *and* it wins, we are done
    (if (and (car boards) (endp (cdr boards)) (win (car boards)))
	(values k (car boards))
	(play-game2 (rest seq) (delete-if #'win boards)))))


(defun solve2 (input)
  (solve input #'play-game2))

    
