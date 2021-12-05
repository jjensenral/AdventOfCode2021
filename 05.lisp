;;; Advent of Code 2021 Day 5
;;; J Jensen

;;; Requires split-sequence


(defconstant +day4-test-input+ "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
")



(defstruct line
  (start #C(0 0))
  (end #C(0 0)))


(defun read-line-from-string (str)
  (when (equal "" str) (return-from read-line-from-string nil))
  (flet ((read-xy (s)
	   (let ((xy (split-sequence:split-sequence #\, s)))
	     (complex (parse-integer (first xy)) (parse-integer (second xy))))))
    (let* ((data (split-sequence:split-sequence #\Space str)))
      (unless (equal (second data) "->") (error "expected \"->\" in the middle"))
      (make-line :start (read-xy (first data)) :end (read-xy (third data))))))


(defun line-delta (line)
  "Give the step from start point to end point, if there is one"
  (let ((d (- (line-end line) (line-start line))))
    (unless (or (zerop (realpart d))
		(zerop (imagpart d))
		(= (abs (realpart d)) (abs (imagpart d))))
      (error "step for ~S is not horiz/vert/diag" line))
    (complex (signum (realpart d)) (signum (imagpart d)))))


(defun line-horiz-or-vert-p (line)
  "Is line horizontal or vertical?"
  (or
   (= (realpart (line-start line)) (realpart (line-end line)))
   (= (imagpart (line-start line)) (imagpart (line-end line)))))


(defun read-lines-from-stream (s)
  (loop for l = (read-line s nil)
	while l
	collect (read-line-from-string l)))


(defun find-corners (lines)
  "Given a list of lines, return the top left and lower right corners"
  ;; mapreducing - not most efficient implementation but this is not critical
  (values
   (complex
    (reduce #'min lines :key (lambda (h) (realpart (line-start h))))
    (reduce #'min lines :key (lambda (h) (imagpart (line-start h)))))
   (complex
    (reduce #'max lines :key (lambda (h) (realpart (line-start h))))
    (reduce #'max lines :key (lambda (h) (imagpart (line-start h)))))))



;;; Note if you pretty-print a diagram, the array will likely come out
;;; transposed, ie with the columns as rows

(defstruct diagram
  (offset 0)
  (board #2A((0 0) (0 0)) :type (array integer (* *))))


(defun diagram-incf (d pt)
  "Increment point pt in diagram d"
  (declare (type diagram d))
  (let ((p1 (- pt (diagram-offset d))))
    (incf (aref (diagram-board d) (realpart p1) (imagpart p1))))
  pt)


(defun mark-line (line diag)
  "Mark a line by incrementing the count in the diagram, returning the number of points incremented"
  (do* ((ct 0 (1+ ct))
	(delta (line-delta line))
	(end (+ (line-end line) delta))	; we need to increment the end point too
	(x (line-start line) (+ x delta)))
       ((= x end) ct)
    (diagram-incf diag x)))


(defun make-diagram-from-lines (lines)
  "Given a list of lines, return a diagram that will accommodate all"
  (multiple-value-bind (topl botr) (find-corners lines)
    ;; Add 1,1 to accommodate the bottom right corner as well
    (let ((dims (+ #C(1 1) (- botr topl))))
      (declare (ignore dims))
      ;; Just keep it simple for now and make it 1000x1000
      (make-diagram :offset 0 :board (make-array '(1000 1000) :initial-element 0)))))



(defun solve-stream (s selector scoref)
  "Given a stream, read lines and plot lines selected by predicate selector, returning the value of calling the score function on the final diagram"
  (let* ((lines (read-lines-from-stream s))
	 (diag (make-diagram-from-lines lines)))
    (dolist (l lines (funcall scoref diag))
      (when (funcall selector l) (mark-line l diag)))))


(defun solve-input (input selector scoref)
  "Given input, and a predicate selecting which lines to apply, return the final diagram"
  (etypecase input
    (pathname (with-open-file (f input :direction :input :if-does-not-exist :error) (solve-stream f selector scoref)))
    (stream (solve-stream input selector scoref))
    (string (solve-stream (make-string-input-stream input) selector scoref))))



(defun score1 (diag)
  "Count the number if entries >1 in the diagram"
  (let* ((dims (array-dimensions (diagram-board diag)))
	 (y (make-array (reduce #'* dims) :displaced-to (diagram-board diag))))
    (count-if (lambda (k) (> k 1)) y)))



(defun solve1 (input)
  (solve-input input #'line-horiz-or-vert-p #'score1))



(defun solve2 (input)
  (solve-input input (constantly t) #'score1))
