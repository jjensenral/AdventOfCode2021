;;; Common support functions for AoC
;;;
;;; Requires split-sequence and regex


(defun process-stream (reduce-func map-func s)
  "Read a stream and mapreduce it"
  (funcall reduce-func
	   (loop for line = (read-line s nil)
		 while line
		 collect (funcall map-func line))))


(defun process-file (reduce-func map-func filename)
  "Read contents of a file into a list and mapreduce it - the reduce function is called with the list as a single argument"
  (with-open-file (foo filename :direction :input
				:if-does-not-exist :error)
    (process-stream reduce-func map-func foo)))



(defun process-file-or-string (reduce-func map-func file-or-string)
  "Process a file (by name) or stream or string"
  (cond
    ((pathnamep file-or-string) (process-file reduce-func map-func file-or-string))
    ((streamp file-or-string) (process-stream reduce-func map-func file-or-string))
    ((stringp file-or-string) (process-stream reduce-func map-func (make-string-input-stream file-or-string)))
    (t (error "Cannot process type ~A" (type-of file-or-string)))))



(defun make-string-parser (&optional syntax)
  "Generate a function to parse a string"
  (declare (ignore syntax))
  ;; cheap and cheerful implementation for now
  (lambda (string)
    (mapcar #'read-from-string (split-sequence:split-sequence #\Space string))))


