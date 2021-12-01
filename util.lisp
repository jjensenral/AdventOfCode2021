;;; Common support functions for AoC

(defun process-file (reduce-func map-func filename)
  "Read contents of a file into a list and mapreduce it"
  (with-open-file (foo filename :direction :input
				:if-does-not-exist :error)
    (funcall reduce-func
	     (loop for line = (read-line foo nil)
		   while line
		   collect (funcall map-func line)))))

