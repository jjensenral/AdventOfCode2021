;;; Advent of Code 2021 Day 6
;;; J Jensen

;;; Requires split-sequence

(defconstant +day6-test-input+ "3,4,3,1,2")



(defun read-input-from-string (s)
  (mapcar #'parse-integer (split-sequence:split-sequence #\, s)))


(defun make-fish-pond (input)
  "Given a list of fish, make a pond"
  (make-array (length input) :adjustable t :initial-contents input :fill-pointer t))


(defun add-to-pond (pond num)
  "Add a number of fish to the end of the pond"
  (let* ((old-len (length pond))
	 (new-len (+ num old-len)))
    (adjust-array pond new-len :fill-pointer t) ;not using fill ptr after all...
    (fill pond 8 :start old-len))
  pond)



(defun fish-day (pond)
  "Update a fish-day"
  (let ((new-fish 0))
    (flet ((update-fish (k)
	     (if (zerop k) (prog1 6 (incf new-fish)) (1- k))))
      (map-into pond #'update-fish pond))
    (add-to-pond pond new-fish)))



(defun sim-fish (input days)
  "From input (a list) simulate days"
  (let ((pond (make-fish-pond input)))
    (dotimes (d days)
;      (declare (ignore d))
      (setq pond (fish-day pond)))
    pond))



(defun solve-string (s &optional (days 80))
  "With a string input, simulate the pond and return the score, the number of fish"
  (length (sim-fish (make-fish-pond (read-input-from-string s)) days)))



(defun solve (s &optional (days 80))
  (etypecase s
    (string (solve-string s days))
    (pathname (with-open-file (f s :direction :input :if-does-not-exist :error) (solve-string (read-line f nil))))
    (stream (read-line s nil) days)))
