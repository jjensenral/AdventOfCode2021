;;; Advent of Code Day 13
;;; J Jensen


;;; Part 1 - data

;;; Implementation notes.
;;;
;;; The first thing I thought of was to use integers as bit vectors to
;;; store each row (with the LSB being the leftmost entry); and store
;;; the rows in an array.  It's a bit more fun than just using a 2D
;;; array.  This is quite a fun problem...

(defconstant +day13-test-input+ "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
")


(defstruct data 
  (size 0)
  (rows #())
  (fold ()))


;; Quite a long function but everything is pretty straightforward...


(defun read-data (input)
  (etypecase input
    (pathname (with-open-file (foo input :direction :input :if-does-not-exist :error) (read-data foo)))
    (string (read-data (make-string-input-stream input)))
    (stream
     (let ((s (make-data :rows (make-array 1 :adjustable t :initial-element 0))))
       ;; read1 reads the first kind of line, entering it into s
       (flet ((read1 (line)
		(let* ((w (mapcar #'parse-integer (split-sequence:split-sequence #\, line)))
		       (x (first w))
		       (y (second w)))
		  (when (>= y (array-dimension (data-rows s) 0))
		    (adjust-array (data-rows s) (1+ y) :initial-element 0))
		  (setf (aref (data-rows s) y) (logior (aref (data-rows s) y) (ash 1 x))
			(data-size s) (max (data-size s) (1+ x)))))
	      ;; read2 reads the second kind of line, could have used regex but hey
	      (read2 (line)
		;; Hack to strip "fold along "
		(let* ((fred (string-left-trim "fold ang" line))
		       (wilma (split-sequence:split-sequence #\= fred)))
		  ;; Turn the string into a char and the number part into a, er, number
		  (push (cons (char (first wilma) 0) (parse-integer (second wilma))) (data-fold s)))))
	 ;; Now with these functions let's read the data
	 (loop with reader = #'read1
	       for line = (read-line input nil)
	       while line
	       if (equal line "")
		 do (setq reader #'read2)
	       else do (funcall reader line)))
       ;; Put the folds into the right order
       (setf (data-fold s) (nreverse (data-fold s)))
       s))))






;; Part 2 - horizontal and vertical flips


;;; Reverse an integer (in bits)
;;; Results are unspecified if there are more bits in the integer than fit in a size
;;; (e.g. a #B11100 is not a four bit integer), or is negative.
;;; Written to be tail recursive.  RT code at end of file.


(defun reverse-number (num size &optional (result 0))
  "Reverse (binary) digits in number"
  (if (zerop size)
      result
      (reverse-number (ash num -1) (1- size) (logior (ash result 1) (if (oddp num) 1 0)))))


(defun fold-integer (x size)
  "Fold an integer of size size into half, returning the number and the count of the overlapping bits"
  (unless (oddp size) (error "cannot fold even number"))
  (let* ((new-size (ash size -1))
	 ;; Note the low end is the left half (in the diagrams)
	 (low (byte new-size 0))
	 (mid (byte 1 new-size))
	 (hi (byte new-size (1+ new-size)))
	 (left-part (ldb low x))
	 (right-part (reverse-number (ldb hi x) new-size)))
    (unless (zerop (ldb mid x))
      (error "Cannot fold: fold line not empty: ~X ~D" x size))
    ;; Now we just need to put the left and right parts together
    (values
     (logior left-part right-part)
     (logcount (logand left-part right-part)))))




(defun fold-along-x (s check)
  (declare (type data s))
  "Fold along x, updating the structure accordingly and returning the total sum of the overlapping bits"
  (unless (= check (ash (data-size s) -1))
    (error "fold-x check failed: fold at ~D, instruction says ~D" (ash (data-size s) -1) check))
  (let ((c 0))
    (map-into (data-rows s)
	      (lambda (k) (multiple-value-bind (result count) (fold-integer k (data-size s))
			    (incf c count)
			    result))
	      (data-rows s))
    (setf (data-size s) (ash (data-size s) -1)) ; this is checked by fold-integer
    c))



(defun fold-along-y (s check)
  (declare (type data s))
  "Fold along y, updating the structure accordingly and returning the total sum of the overlapping bits"
  (let* ((c 0)
	 (size (array-dimension (data-rows s) 0)) ; aka length
	 (new-size (ash size -1))) ; size <- (size-1)/2
    (unless (= new-size check) (error "fold-y along ~D, instruction says ~D" new-size check))
    (unless (oddp size) (error "cannot fold unless odd number of rows"))
    (unless (zerop (aref (data-rows s) new-size))
      (error "cannot fold as fold line ~D is not empty - ~X" new-size (aref (data-rows s) new-size)))
    (loop for i from 0 below new-size 
	  do (incf c (logcount (logand (aref (data-rows s) i) (aref (data-rows s) (- size i 1)))))
	  do (setf (aref (data-rows s) i) (logior (aref (data-rows s) i) (aref (data-rows s) (- size i 1)))))
    (adjust-array (data-rows s) new-size)
    c))





;; Part 3 - more folding logic, scoring

(defun visible-dots (s)
  "Count the number of visible 'dots' in the current layout"
  (declare (type data s))
  (apply #'+ (map 'list #'logcount (data-rows s))))



(defun fold (s)
  "Run all the fold instructions, updating the structure as we go"
  (when (endp (data-fold s)) (return-from fold s))
  (let ((instr (pop (data-fold s))))
    (funcall (if (eql (car instr) #\x) #'fold-along-x #'fold-along-y) s (cdr instr)))
  (fold s))


(defun print-data (s &optional (out *standard-output*))
  (declare (type data s) (type stream out))
  "Print the array as in the puzzle description"
  (labels ((print-num (num length)
	     "Print number of given length, LSB first"
	     (when (plusp length)
	       (write-char (if (oddp num) #\# #\.) out)
	       (print-num (ash num -1) (1- length))))
	   (print-line (num)
	     (print-num num (data-size s))
	     (terpri out)))
    (map nil #'print-line (data-rows s)))
  s)





;; Part 4 - solvers


(defun solve1 (input)
  (let ((data (read-data input)))
    ;; remove all but the first folder instruction
    (rplacd (data-fold data) nil)
    (visible-dots (fold data))))


(defun solve2 (input)
  (let ((data (read-data input)))
    (print-data (fold data))))





;; Part $ - some test code


#+rt
(rt:deftest (13 reverse-number 1)
    (reverse-number 12 4)
    3)

#+rt
(rt:deftest (13 reverse-number 2)
    (reverse-number 3 4)
    12)

#+rt
(rt:deftest (13 reverse-number 3)
    (reverse-number #B10001110111100111100001001010 30)
    #B10100100001111001111011100010)

#+rt
(rt:deftest (13 reverse-number 4)
    (reverse-number 0 4)
    0)

#+rt
(rt:deftest (13 reverse-number 5)
    (reverse-number 1 4)
    #B1000)


;; This will test whether the read-data function works, but doesn't
;; tell you what's wrong if it isn't.  The reason is that we need
;; equalp and rt uses #'EQUAL.

#+rt
(rt:deftest (13 read-data 1)
    (equalp (read-data +day13-test-input+)
	    #S(DATA
	       :SIZE 11
	       :ROWS #(584 16 0 1 1288 0 0 0 0 0 834 16 1088 1 5)
	       :FOLD ((#\y . 7) (#\x . 5))))
  t)


#+rt
(rt:deftest (13 fold-integer 1)
    (fold-integer #B1110110 7)
  7 2)
