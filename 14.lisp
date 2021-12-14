;;; Advent of Code 2021 Day 14
;;; J Jensen


;; Part 1 - input

(defconstant +day14-test-input+ "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
")


(defun char-to-symbol (c)
  (declare (type character c))
  "Create a symbol with a single character name"
  (intern (make-string 1 :initial-element c)))



(defun read-input (input &aux rules)
  (etypecase input
    (string (read-input (make-string-input-stream input)))
    (pathname (with-open-file (foo input :direction :input :if-does-not-exist :error) (read-input foo)))
    (stream
     (values
      ;; first line - a list of char
      (map 'list #'char-to-symbol (read-line input nil))
      (flet ((add-rule (x y z)
	       (if (assoc x rules)
		   (push (cons y z) (cdr (assoc x rules)))
		   (push (list x (cons y z)) rules))))
	;; second line - empty (value discarded)
	(unless (equal (read-line input nil) "") (error "Expected empty second line"))
	;; everything else
	(loop for line = (read-line input nil)
	      while line
	      do (add-rule (char-to-symbol (char line 0))
			   (char-to-symbol (char line 1))
			   (char-to-symbol (char line 6))))
	rules)))))




;; Part 2 - list building


(defun lookup-rule (x y rules)
  (cdr (assoc y (cdr (assoc x rules)))))



(defun nupdate-list (input rules)
  "Update list according to the rules, modifying list"
  (do ((m input (cddr m)))
      ((endp (cdr m)) input)
    (let* ((a (lookup-rule (first m) (second m) rules)))
      ;; Insert a unless it is NIL
      (unless a (cerror "Continue" "Rule lookup failed for ~S" m))
      (rplacd m (cons a (cdr m)))))
  input)

;; For part 2, we're back to counting (or linear algebra).  Probably the best
;; approach is to count pairs, so a list now is of the form
;; (((C . B) . 1) ((N . C) . 1) ((N . N) . 1))


(defmacro incf-list2-entry (list x y delta)
  "Create or add delta to the entry in list for the pair x y"
  (let ((bzz (gensym)) (garg (gensym)))
    `(let* ((,bzz (cons ,x ,y))
	    (,garg (assoc ,bzz ,list :test #'equal)))
       (if ,garg
	   (incf (cdr ,garg) ,delta)
	   (setf ,list (acons ,bzz ,delta ,list)))
       ,list)))


;; (convert-list (list 'n 'n 'c 'b))
;; => (((C . B) . 1) ((N . C) . 1) ((N . N) . 1))


(defun convert-list (list &aux new-list)
  "From first half of puzzle list to second half of puzzle list format"
  (mapl (lambda (data)
	  ;; If we have two or more elements...
	  (unless (endp (cdr data))
	    (incf-list2-entry new-list (first data) (second data) 1)))
	list)
  new-list)


(defun update-list2 (list rules &optional (result nil))
  "Update list (of type list2, ie for 2nd half of puzzle) according to rules"
  ;; can't update in place or we will do next round before we finish the current one
  (if (endp list)
      result
      (progn
	(let* ((curr (first list))
	       (pair (car curr))
	       (count (cdr curr))
	       (new (lookup-rule (car pair) (cdr pair) rules)))
	  (incf-list2-entry result (car pair) new count)
	  (incf-list2-entry result new (cdr pair) count))
	(update-list2 (rest list) rules result))))



;; Part 3 - score

(defun concordance1 (list)
  (let ((elts (remove-duplicates list)))
    (map-into elts (lambda (e) (cons e (count e list))) elts)))


(defun score1 (list)
  (let* ((y (sort (concordance1 list) #'< :key #'cdr))
	 (min (first y))
	 (max (first (last y))))
    (- (cdr max) (cdr min))))


(defun concordance2 (list2 &optional (result nil))
  "Return an alist of counts of the elements"
  (if (endp list2)
      result
      (progn
	(macrolet ((add-to-pos (place item count)
		     (let ((pair (gensym)))
		       `(let ((,pair (assoc ,item ,place)))
			  (if ,pair
			      (incf (cdr ,pair) ,count)
			      (push (cons ,item ,count) ,place))))))
	  (let* ((entry (first list2))
		 (count (cdr entry)))
	    (add-to-pos result (caar entry) count) ; first half of pair
	    (add-to-pos result (cdar entry) count))) ; second half
	(concordance2 (rest list2) result))))


;; Since we are counting pairs, every element gets counted twice except
;; the ones at the beginning and end.

(defun score2 (list2 first last)
  (let* ((count (concordance2 list2)))
    (incf (cdr (assoc first count)))
    (incf (cdr (assoc last count)))
    ;; divide all by two
    (mapc (lambda (cell) (setf (cdr cell) (ash (cdr cell) -1))) count)
    (let* ((sorted-count (sort count #'< :key #'cdr))
	   (least (first sorted-count))
	   (most (first (last sorted-count))))
      (- (cdr most) (cdr least)))))



;; Part 4 - solvers

(defun solve1 (input)
  (multiple-value-bind (m r) (read-input input)
    (dotimes (i 10)
      (nupdate-list m r))
    (score1 m)))


(defun solve2 (input)
  (multiple-value-bind (m rules) (read-input input)
    ;; keep the first and last elements of input for accounting purposes
    ;; (these never change)
    (let ((first (first m))
	  (last (first (last m))))
      (labels ((generation (list2 num)
		 (if (zerop num)
		     list2
		     (generation (update-list2 list2 rules) (1- num)))))
	(score2 (generation (convert-list m) 40) first last)))))
