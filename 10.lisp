;;; Advent of Code 2021
;;; Day 10
;;; J Jensen


(defconstant +day10-test-input+ "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
")


(defconstant +delims+ '((#\[ . #\]) (#\( . #\)) (#\< . #\>) (#\{ . #\}))
  "Matching pairs of delimeters")



(defun parse-helper (chars stack)
  "Return the final stack or the mismatching character"
  (let ((c (car chars)))
    (cond
      ;; done?
      ((null c) stack)
      ;; opening delimeter?
      ((member c +delims+ :key #'car)
       (parse-helper (cdr chars) (cons c stack)))
      ;; closing delimeter, but is it matching the top of stack?
      ((eql c (cdr (assoc (car stack) +delims+ :test #'eql)))
       (parse-helper (cdr chars) (cdr stack)))
      (t c))))



(defun parse-string (s)
  ;; "explode" the string
  (parse-helper (coerce s 'list) nil))



(defun solve1-stream (s)
  (let* ((input (read-line s nil))
	 (result (and input (parse-string input))))
    (cond
      ((null input) 0)
      ((characterp result)
       (+  (cdr (assoc result '((#\) . 3) (#\] . 57) (#\} . 1197) (#\> . 25137)))) (solve1 s)))
      (t (solve1 s)))))


(defun solve-input (input solv-func)
  (etypecase input
    (stream (funcall solv-func input))
    (pathname (with-open-file (foo input :direction :input :if-does-not-exist :error) (solve-input foo solv-func)))
    (string (solve-input (make-string-input-stream input) solv-func))))


(defun solve1 (input)
  (solve-input input #'solve1-stream))


(defun score2 (string)
  "Calculate score for the second half of the contest of a given string"
  (let ((elts (parse-string string)))
    (if (characterp elts)
	nil
	(reduce (lambda (a b) (+ (* a 5) b))
		(mapcar (lambda (c) (position c " ([{<")) elts)))))


(defun solve2-stream (s)
  "Solve second half"
  (labels ((scores (s)
	     (let ((y (read-line s nil)))
	       (if y (cons (score2 y) (scores s)) nil))))
    (let* ((all-scores (delete-if #'null (scores s)))
	   (srt-scores (sort all-scores #'<))
	   (len-scores (length srt-scores))
	   (mid-score (/ (1- len-scores) 2)))
      (unless (integerp mid-score)
	(error "Odd number of scores ~D" len-scores))
      (nth mid-score srt-scores))))


(defun solve2 (input)
  (solve-input input #'solve2-stream))
