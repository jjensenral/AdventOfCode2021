;;; Advent of Code 2021 Day 3
;;; J Jensen



(defconstant +day3-test-input+ "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
")



(defun read-numbers (data)
  "Read numbers and return them plus the number of bits and an integer with all bits set.  The length is not checked, using just the length of the first line."
  (let* ((strings (process-file-or-string #'identity #'identity data))
	 (numbers (mapcar (lambda (s) (parse-integer s :radix 2)) strings))
	 (length (length (first strings))))
    (values numbers length (1- (ash 1 length)))))



(defun bits-to-number (bits)
  "Given a list of bits, return the number they represent (MSB first)"
  (reduce (lambda (a b) (logior (ash a 1) b)) bits))



(defun solve1 (data)
  (multiple-value-bind (nums len all) (read-numbers data)
    (let* ((half-count (floor (length nums) 2))
	   (bitct (loop for bit downfrom (1- len) to 0
			collect (count-if (lambda (k) (logbitp bit k)) nums)))
	   (gamma (bits-to-number (mapcar (lambda (n) (if (> n half-count) 1 0)) bitct))))
      (* gamma (- all gamma)))))



(defun xor (loga logb)
  "Logical xor"
  (declare (type boolean loga logb))
  (if loga (not logb) logb))



(defun nbit-filter (numbers j keep)
  "Filter bit j keeping the most common or least common; the numbers list is altered"
  ;; half-count is rational in general
  (let* ((half-count (/ (length numbers) 2))
	 (byte-spec (byte 1 j))
	 ;; counting numbers with ones at position j
	 (c (count-if (lambda (k) (ldb-test byte-spec k)) numbers))
	 ;; Which bit should we delete?
	 (delbit (cond
		   ((>= c half-count) (if (eq keep 'most) 0 1))
		   (t (if (eq keep 'most) 1 0)))))
    (delete delbit numbers :key (lambda (k) (ldb byte-spec k)))))



(defun keep-reducing (numbers j keep)
  "Run nbit-filter on data (starting at bit j) until there is only one entry left"
  (do ((bitno j (1- bitno))
       (num (copy-seq numbers) (nbit-filter num bitno keep)))
      ((or (endp (cdr num)) (zerop j)) num)))




(defun solve2 (data)
  (multiple-value-bind (nums len all) (read-numbers data)
    (declare (ignore all))
    (let ((a (keep-reducing nums (1- len) 'most))
	  (b (keep-reducing nums (1- len) 'least)))
      ;; Check that we got one element lists
      (unless (and a b (not (cdr a)) (not (cdr b)))
	(error "Something went wrong: most=~S least=~S" a b))
      (* (car a) (car b)))))

