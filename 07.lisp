;;; Advent of Code Day 7
;;; J Jensen

;;; Requires split-sequence


(defconstant +day7-test-input+ "16,1,2,0,4,2,7,1,2,14")


(defun read-crabs-from-string (str)
  (mapcar #'parse-integer (split-sequence:split-sequence #\, str)))



(defun fuel-for-move (crabs pos)
  "Calculate the fuel required for a move to position pos"
  (apply #'+ (mapcar (lambda (j) (abs (- pos j))) crabs)))



(defun search-min-pos (crabs fuel-func)
  "Find a minimum fuel for crabs"
  (let* ((cmin (apply #'min crabs))
	 (cmax (apply #'max crabs)))
	 ;; simple method, though an actual search may be feasible
    (loop for pos from cmin upto cmax minimizing (funcall fuel-func crabs pos))))



(defun solve-input (input fuel-func)
  (etypecase input
    (string (search-min-pos (read-crabs-from-string input) fuel-func))
    (stream (solve-input (read-line input nil) fuel-func))
    (pathname (with-open-file (f input :direction :input :if-does-not-exist :error) (solve-input f fuel-func)))))



(defun solve1 (input)
  (solve-input input #'fuel-for-move))



(defun fuel-for-move-2 (crabs pos)
  "Calculate the fuel required for a move to position pos"
  (flet ((triangle-number (k) (/ (* k (1+ k)) 2)))
    (apply #'+ (mapcar (lambda (j) (triangle-number (abs (- pos j)))) crabs))))


(defun solve2 (input)
  (solve-input input #'fuel-for-move-2))
