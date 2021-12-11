;;; Advent of Code 2021
;;; Day 11
;;; J Jensen

;;; Requires util.lisp (which requires split-sequence)

(defconstant +day11-test-input+ "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
")




(defun read-input (input)
  (let ((data (process-file-or-string #'identity (lambda (x) (integer-digits (parse-integer x))) input)))
    (unless (apply #'= (mapcar #'length data)) (error "Unequal lengths"))
    (make-array (list (length data) (length (first data))) :initial-contents data)))



;;; Octopus Generation


(defun flash (a x y)
  "Helper function to \"flash\" at row x column y"
  (loop for delta in '((1 . -1) (1 . 0) (1 . 1) (0 . -1) (0 . 1) (-1 . -1) (-1 . 0) (-1 . 1))
	as x1 = (+ x (car delta))
	as y1 = (+ y (cdr delta))
	when (array-in-bounds-p a x1 y1)
	  do (incf (aref a x1 y1)))
  (setf (aref a x y) 0))		; prevent retriggering (its neighbours can increment only to 8)


(defun incf-array (a)
  ;; not the height of elegance but it'll do
  (dotimes (k (apply #'* (array-dimensions a)))
    (incf (row-major-aref a k))))


(defun find-flasher (a f)
  "Return a cons cell with the coordinates of the (first) flashing octopus, or nil if there is none"
  ;; again not a very elegant implementation
  (loop for r from 0 below (array-dimension a 0)
	do (loop for c from 0 below (array-dimension a 1)
		 when (and (not (aref f r c)) (> (aref a r c) 9))
		   do (return-from find-flasher (cons r c))))
  nil)


(defun octogen (a)
  "Octopus generation - modifies array and returns number of flashes"
  (let ((flashed (make-array (array-dimensions a) :initial-element nil :element-type 'boolean)))
    ;; part 1: increment all
    (incf-array a)
    ;; part 2: flash the flashers
    (loop as w = (find-flasher a flashed)
	  while w
	  do (flash a (car w) (cdr w))
	  do (setf (aref flashed (car w) (cdr w)) t))
    ;; part 3: re-zero the flashers
    (let ((c 0))
      (dotimes (k (apply #'* (array-dimensions a)))
	(when (row-major-aref flashed k)
	  (setf (row-major-aref a k) 0) (incf c)))
      c)))



(defun solve1 (input &optional (gens 100))
  (let ((a (read-input input)))
    (labels ((run-gen (k)
	       (if (zerop k) 0
		   (+ (octogen a) (run-gen (1- k))))))
      (run-gen gens))))


(defun solve2 (input)
  (let* ((a (read-input input))
	 (all (apply #'* (array-dimensions a)))
	 (g 1))
    (loop until (= (octogen a) all)
	  do (incf g))
    g))
