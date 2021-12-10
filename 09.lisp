;;;; Advent of Code 2021
;;; Day 9
;;; J Jensen


;;; Can we avoid a boring "for every point let's check all the neighbours" implementation?
;;; Let's try...


(defconstant +day9-test-input+ "2199943210
3987894921
9856789892
8767896789
9899965678
")


;; This would be the cleverer implementation (using integer-digits from the util
;; file) but it would drop leading zeros from the input (as it happens, there aren't
;; any, but it's the principle...)

#|
(defun read-input (s)
  (declare (type stream s))
  (let ((line (read-line s nil)))
    (if line (cons (integer-digits (read-from-string line)) (read-input s))
	nil)))
|#

(defun read-input (s)
  (declare (type stream s))
  (let* ((chars '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4) (#\5 . 5) (#\6 . 6) (#\7 . 7) (#\8 . 8) (#\9 . 9)))
	 (data (loop as y = (read-line s nil)
		     while y
		     collecting (map 'list (lambda (c) (cdr (assoc c chars))) y)))
	 (lengths (mapcar #'length data)))
    (unless (apply #'= lengths)
      (error "Lines of unequal length ~S" lengths))
    data
    ;;  (make-array (list (length lengths) (first lengths)) :initial-contents data)
    ))


;;; This is slightly clever if I may say so myself: we guard the row by
;;; putting large numbers at the end; then run a check-for-piggy-in-the-middle
;;; against each window of three consecutive digits by mapping across the list
;;; offset 0, 1 and 2 times.  Unit tests at the end of the file.


(defun row-troughs (row)
  "Return the locations of the \"troughs\" in a row of numbers, local minima"
  ;; General case: haven't thought too much about it
  (let* ((ext-row (cons most-positive-fixnum (append row (cons most-positive-fixnum nil))))
	 (idx 0))
    (mapcan (lambda (a b c) (prog1 (if (and (> a b) (< b c)) (list idx) nil) (incf idx)))
	  ext-row (cdr ext-row) (cddr ext-row))))


(defun find-troughs-rows (rows &optional (rnum 0))
  "Return cons cells with (r . c) of rows and columns with suspected (row-local) minima"
  (if (endp rows) nil
      (nconc (mapcar (lambda (c) (cons rnum c)) (row-troughs (first rows)))
	     (find-troughs-rows (rest rows) (1+ rnum)))))


;;; Transpose a list of lists by mapping across the lists - unit tests
;;; at end of file.  This Magic works because apply accepts extra
;;; parameters for the function before the lists.

(defun transpose (rows)
  "Transpose rows and columns, list of lists version"
  (apply #'mapcar #'list rows))


(defun nswap-car-cdr (alist)
  "Given an association list, swap the car and cdr of each entry in place"
  (mapc (lambda (cell) (rotatef (car cell) (cdr cell))) alist)
  alist)


(defun solve-stream-mimima (s)
  "Return local minima for a stream of data, and the data"
  (let* ((data (read-input s))
	 (row-min (find-troughs-rows data))
	 ;; Find column troughs by swapping rows and cols, then swap them
	 ;; back again once the results are known
	 (col-min (nswap-car-cdr (find-troughs-rows (transpose data))))
	 ;; Find elements that are minima for both rows and cols
	 (mins (nintersection col-min row-min :test #'equal)))
    (values mins data)))



(defun score1 (minima data)
  "Calculate score in part 1"
    ;;; Now gather the data, slightly inefficient lists lookup could be
    ;;; solved by creating an array
  (reduce #'+ (mapcar (lambda (row-col) (nth (cdr row-col) (nth (car row-col) data))) minima) :initial-value (length minima)))





(defun solve1 (input)
  (etypecase input
    (stream (multiple-value-bind (m d) (solve-stream-mimima input) (score1 m d)))
    (pathname (with-open-file (foo input :direction :input :if-does-not-exist :error) (solve1 foo)))
    (string (solve1 (make-string-input-stream input)))))


;;; From now on we better stop being clever and make the data an array
;;; Basin can be a list of points as conses

(defun grow-basin (basin-todo data &optional (basin-done nil))
  "Attempt to grow a basin"
  (let ((f (pop basin-todo)))
    (if f
	(flet ((maybe-add (pt dir)
		 (let ((pt1 (cons (+ (car pt) (car dir)) (+ (cdr pt) (cdr dir)))))
		   ;; let's hope all basins are bounded by lucky nines...
		   (unless (or (not (array-in-bounds-p data (car pt1) (cdr pt1)))
			       (eql (aref data (car pt1) (cdr pt1)) 9)
			       (member pt1 basin-todo :test #'equal)
			       (member pt1 basin-done :test #'equal))
		     (push pt1 basin-todo)))))
	  (maybe-add f '(0 . 1))
	  (maybe-add f '(0 . -1))
	  (maybe-add f '(1 . 0))
	  (maybe-add f '(-1 . 0))
	  (grow-basin basin-todo data (cons f basin-done)))
	(length basin-done))))


(defun solve2-stream (s)
  (multiple-value-bind (mins data) (solve-stream-mimima s)
    ;; Replace the list of lists with the array form, discarding lists
    (setq data (make-array (list (length data) (length (first data))) :initial-contents data))
    (mapcar (lambda (min) (grow-basin (list min) data nil)) mins)))


(defun score2 (list)
  (let ((sorted-list (sort list #'>)))
    (* (first sorted-list) (second sorted-list) (third sorted-list))))


(defun solve2 (input)
  (etypecase input
    (stream (score2 (solve2-stream input)))
    (pathname (with-open-file (foo input :direction :input) (solve2 foo)))
    (string (solve2 (make-string-input-stream input)))))
	 




;;; row-troughs test functions


;;; Test driven development?  Maybe we shouldn't worry about the special cases
#+rt
(rt:deftest (9 row 1)
    (row-troughs '(1 2 3 4 3 2 1))
  (0 6))

#+rt
(rt:deftest (9 row 2)
    (row-troughs '(2 1 9 9 9 4 3 2 1 0))
  (1 9))

#+rt
(rt:deftest (9 row 3)
    (row-troughs '(4 3 1 2 4 2 5 7))
  (2 5))

#+rt
(rt:deftest (9 row 4)
    (row-troughs '(2 3 1))
  (0 2))

#+rt
(rt:deftest (9 row 5)
    (row-troughs '(3 1 2))
  (1))

#+rt
(rt:deftest (9 row 6)
    (row-troughs '(1 2))
  (0))

#+rt
(rt:deftest (9 row 7)
    (row-troughs '(2 1))
  (1))

#+rt
(rt:deftest (9 row 8)
    (row-troughs nil)
  nil)



;;; transpose test functions



#+rt
(rt:deftest (9 transpose 1)
    (transpose '((1 2) (3 4)))
  ((1 3) (2 4)))

#+rt
(rt:deftest (9 transpose 2)
    (transpose '((1 2 3 4) (5 6 7 8)))
  ((1 5) (2 6) (3 7) (4 8)))

#+rt
(rt:deftest (9 transpose 3)
    (transpose '((1 5) (2 6) (3 7) (4 8)))
  ((1 2 3 4) (5 6 7 8)))

#+rt
(rt:deftest (9 transpose 4)
    (transpose '((1)))
  ((1)))
