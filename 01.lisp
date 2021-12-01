;;;; AoC 2021
;;;; Jens Jensen


(defun solve1 (data)
  "Count number of numbers that increase in a list"
  (count-if #'plusp (mapcar #'- (cdr data) data)))


#+rt
(rt:deftest (1 . 1) (solve1 (list 199 200 208 210 200 207 240 269 260 263))
  7)

(defun solve1-file ()
  "run solve1 on input file"
  (process-file #'solve1 (lambda (x) (parse-integer x)) #P"01.input"))


(defun sum-3-window (list)
  "For a list of inputs, return the sum of sliding length 3 windows"
  (mapcar (lambda (a b c) (+ a b c)) list (cdr list) (cddr list)))


(defun solve2 (data)
  (solve1 (sum-3-window data)))


#+rt
(rt:deftest (1 . 2) (solve2 (list 199 200 208 210 200 207 240 269 260 263))
  5)


(defun solve2-file ()
  "Run solve2 on the input file"
  (process-file #'solve2 (lambda (x) (parse-integer x)) #P"01.input"))
