;;; Advent of Code 2021
;;; Day 8
;;; J Jensen

;;; Requires split-sequence and also uses map-permutations from Alexandria
;;; 

(defun parse-stream (s)
  (let ((line (read-line s nil)))
    (if line
	(cons (mapcar (lambda (part) (split-sequence:split-sequence #\Space part :remove-empty-subseqs t)) (split-sequence:split-sequence #\| line))
	      (parse-stream s))
	nil)))

(defconstant +day8-test-input+ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
")


(defun solve1-input (s)
  (declare (type stream s))
  ;; Discard first half of input data"
  (let* ((data (mapcar #'second (parse-stream s)))
	 (lengths (mapcar #'length (apply #'nconc data))))
    ;; 1 4 7 8
    (+ (count 2 lengths) (count 4 lengths) (count 3 lengths) (count 7 lengths))))

    
(defun solve-input (input solve-func)
  (etypecase input
    (string (funcall solve-func (make-string-input-stream input)))
    (pathname (with-open-file (foo input :direction :input :if-does-not-exist :error) (funcall solve-func foo)))
    (stream (funcall solve-func input))))


(defun solve1 (input)
  (solve-input input #'solve1-input))


(defconstant +digits+ '(("abcefg" . 0) ("cf" . 1) ("acdeg" . 2) ("acdfg" . 3) ("bcdf" . 4) ("abdfg" . 5) ("abdefg" . 6) ("acf" . 7) ("abcdefg" . 8) ("abcdfg" . 9)))


(defun is-digit (segs)
  "If a combination of segments given as a string corresponds to a digit, return it"
  (cdr (assoc (sort segs #'char-lessp) +digits+ :test #'equal)))


(defun permute-segments (segs perm)
  "Return a permutation of the segments"
  ;; Creates a new string every time which is Inefficient but we can live with that in this small problem
  (let ((index "abcdefg"))
    ;; The position call can issue a warning because it can return nil...
    (map 'string (lambda (ch) (schar index (nth (the unsigned-byte (position ch index)) perm))) segs)))



(defun check-line (line perm)
  ;; check whether a line deciphers using a particular permutation and throw 'found with the last four digits if it does
  ;; A little bit inefficient not to shortcut when a mismatch is found
  (let* ((permuted-line (mapcar (lambda (s) (permute-segments s perm)) line))
	 (digits (mapcar #'is-digit permuted-line)))
    (when (every #'identity digits)
      (throw 'found (last digits 4)))))



(defun solve2-line (line1 line2)
  "Solve one line of input (given in two parts), returning the last four digits if a solution is found"
  ;; The separation is of no use to us
  (let ((line (append line1 line2)))
    (catch 'found
      (alexandria:map-permutations (lambda (p) (check-line line p)) (loop for i from 0 to 6 collect i) :copy nil))
    ))


(defun solve2-input (s)
  (declare (type stream s))
  (let* ((data (parse-stream s)))
    (mapcar (lambda (pair) (solve2-line (first pair) (second pair))) data)))



(defun solve2 (input)
  (let ((solution (solve-input input #'solve2-input)))
    (apply #'+
	   (mapcar (lambda (digits)
		     (reduce (lambda (a b) (+ (* a 10) b)) digits))
		   solution))))

