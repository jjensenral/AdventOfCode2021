;;; AoC 2021 Day 2

(defconstant +day2-test-input+ "forward 5
down 5
forward 8
up 3
down 8
forward 2
")


;; In the first half, pos is a cons of horizontal position and depth


(defun nrun-cmd (pos cmd)
  "Update pos with instructions from cmd"
  (let ((d (second cmd)))
    (declare (type (integer 0 *) d))
    (ecase (first cmd)
      (forward (incf (car pos) d))
      (down (incf (cdr pos) d))
      (up (decf (cdr pos) d)
       (when (minusp (cdr pos))
	 (cerror "Reset to zero and continue" "Caution: attempt to make sub fly")
	 (setf (cdr pos) 0)))))
  pos)


(defun nrun-cmds (pos cmds run-function)
  "Run all commands in order on pos, altering pos"
  (cond
    ((endp cmds) pos)
    (t (nrun-cmds (funcall run-function pos (first cmds)) (rest cmds) run-function))))


(defun solver (run-function init-pos data)
  (let* ((p (make-string-parser))
	 (input (process-file-or-string #'identity p data))
	 (horiz-depth (nrun-cmds init-pos input run-function)))
    (etypecase horiz-depth
      (cons (* (car horiz-depth) (cdr horiz-depth)))
      (vector (* (svref horiz-depth 0) (svref horiz-depth 1))))))


;;; Run (solve1 +day2-test-input+)
;;; or (solve1 #P"02.input")

(defun solve1 (data)
  (declare (type (or stream pathname string) data))
  (solver #'nrun-cmd (cons 0 0) data))


;;; In the second half, pos is a simple vector #(horiz depth aim)


(defun nrun-cmd2 (pos cmd)
  "Update and return pos based on the second half of the puzzle"
  (let ((d (second cmd)))
    (declare (type (integer 0 *) d))
    (ecase (first cmd)
      (forward
       (incf (svref pos 0) d)
       (incf (svref pos 1) (* d (svref pos 2))))
      (down (incf (svref pos 2) d))
      (up (decf (svref pos 2) d))))
  pos)


(defun solve2 (data)
  (declare (type (or stream pathname string) data))
  (solver #'nrun-cmd2 (make-array 3 :initial-element 0) data))

