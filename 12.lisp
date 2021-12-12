;;; Advent of Code, Day 12
;;; J Jensen

;;; Requires code from util.lisp and split-sequence

;;; Part 1: input data


(defconstant +day12-test-input+ "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
")

(defconstant +day12-input+ "pg-CH
pg-yd
yd-start
fe-hv
bi-CH
CH-yd
end-bi
fe-RY
ng-CH
fe-CH
ng-pg
hv-FL
FL-fe
hv-pg
bi-hv
CH-end
hv-ng
yd-ng
pg-fe
start-ng
end-FL
fe-bi
FL-ks
pg-start
")



;; Part 2: the map implementation


;;; Implementation notes.  This looks like something that should be
;;; done in a lispy way, so we keep "cave" names as symbols.  Note
;;; that the default reader maps symbols to upper case, as all built
;;; in symbols are upper case (a legacy of lisp's ancient origins)
;;; Since we are using Old Lisp today, we might as well use plists
;;; to maintain the map.  We create a special package called map
;;; to hold the symbols.
;;; This is probably not the most efficient way - we can only hold
;;; one map at a time - but hey, it's Sunday, let's have some fun.

(defpackage map)


(defun read-input (input)
  (labels ((make-connection (a b)
	     "Make a connection from point a to b and back again"
	     (push b (get a 'neighbours))
	     (push a (get b 'neighbours)))
	   (process-line (line)
	     "Record the connection described by a single line of input"
	     (let* ((data (split-sequence:split-sequence #\- line))
		    (a (intern (first data) "MAP"))
		    (b (intern (second data) "MAP")))
	       (make-connection a b))))
    (process-file-or-string #'identity #'process-line input)))



(defun clear-map (&optional (prop 'neighbours))
  "Erase the currently held map by clearing neighbours - will only clear the plists, the symbols will remain.  Optionally clear another property."
  (do-symbols (sym "MAP") (remprop sym prop)))




;;; Part 3: algorithm


;; careful: "start" and "end" will be small...

(defun smallp (sym)
  "Is this a \"small\" cave?"
  (let ((name (symbol-name sym)))
    (string= (string-downcase name) name)))


;; The path appears backwards to the callback, e.g.
;; (MAP::|end| MAP::HN MAP::|dc| MAP::HN MAP::|kj| MAP::|start|)

(defun find-path (where-i-am done callback)
  "Simple path tracer calling callback when the end is reached"
  (if (eq 'map::|end| where-i-am)
      (funcall callback done)
      (dolist (maybe-visit (get where-i-am 'neighbours))
	(unless (and (smallp maybe-visit) (member maybe-visit done))
	  (find-path maybe-visit (cons maybe-visit done) callback)))))



(defun for-all-paths (callback find-algo)
  "Generate all paths and call callback on them in turn"
  ;; FIXME hack to support the different parameter list
  (if (eq find-algo #'find-path)
      (funcall find-algo 'map::|start| (list 'map::|start|) callback)
      ;; find-path2 is not defined yet; the SBCL compiler will warn if we
      ;; try to call it now
      (funcall find-algo 'map::|start| (list 'map::|start|) nil callback)))



(defun solve-input (input find-algo)
  (clear-map)
  (read-input input)
  (let ((c 0))
    (for-all-paths (lambda (path)
		     (declare (ignore path))
		     (incf c))
		   find-algo)
    c))


(defun solve1 (input)
  (solve-input input #'find-path))



;;; Part 4 - updated algo for the second half of the puzzle

;;; The adaption is fairly straightforward: the extra parameter says
;;; whether a small cave has been visited twice in the current path
;;; It may look complicated but it worked in the first attempt!

(defun find-path2 (where-i-am done small-cave-twice callback)
  "Simple path tracer calling callback when the end is reached"
  (if (eq 'map::|end| where-i-am)
      (funcall callback done)
      (dolist (maybe-visit (get where-i-am 'neighbours))
	(cond
	  ;; We have already visited a small cave twice: previous logic applies
	  (small-cave-twice
	   (unless (and (smallp maybe-visit) (member maybe-visit done))
	     (find-path2 maybe-visit (cons maybe-visit done) small-cave-twice callback)))
	  ;; cave is large
	  ((not (smallp maybe-visit))
	   (find-path2 maybe-visit (cons maybe-visit done) small-cave-twice callback))
	  ;; We have not visited a small cave twice, and current cave is small
	  ((not (eq maybe-visit 'map::|start|))
	   (let ((previous-visits (count maybe-visit done)))
	     (when (< previous-visits 2)
	       (find-path2 maybe-visit (cons maybe-visit done) (= previous-visits 1) callback))))))))



(defun solve2 (input)
  (solve-input input #'find-path2))
