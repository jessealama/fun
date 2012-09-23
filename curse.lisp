
(in-package :cl-user)

;; Solving the number puzzles from the iPad game "The Curse"

(defparameter *dimension* 4)

(defclass board ()
  ((configuation
    :initarg :configuration
    :reader configuration
    :type (array t)
    :initform (make-array '(4 4) :initial-element nil))
   (target-sum
    :initarg :target-sum
    :reader target-sum
    :type integer
    :initform (error "A target sum is needed!"))))

(defmethod print-object ((board board) stream)
  (let ((config (configuration board))
	(target (target-sum board)))
    (format stream "Target: ~d" target)
    (terpri stream)
    (dotimes (i *dimension*)
      (format stream "|")
      (dotimes (j *dimension*)
	(let ((piece (aref config i j)))
	  (if piece
	      (if (< piece 10)
		  (format stream "  ~d |" piece)
		  (format stream " ~d |" piece))
	      (format stream " -- |")))
	(when (= j (1- *dimension*))
	  (unless (= i (1- *dimension*))
	    (terpri stream)))))))

(defun board-row (board row-number)
  (loop
     with configuration = (configuration board)
     for i from 0 upto (1- *dimension*)
     collect (aref configuration row-number i) into row
     finally (return row)))

(defun board-column (board row-number)
  (loop
     with configuration = (configuration board)
     for i from 0 upto (1- *dimension*)
     collect (aref configuration i row-number) into column
     finally (return column)))

(defun every-row-solved? (board)
  (loop
     with target-sum = (target-sum board)
     for i from 0 upto (1- *dimension*)
     for row = (board-row board i)
     do
       (unless (list-solved? row target-sum)
	 (return nil))
     finally
       (return t)))

(defun list-solved? (list target-sum)
  (unless (member nil list)
    (= (reduce #'+ list) target-sum)))

(defun every-column-solved? (board)
  (loop
     with target-sum = (target-sum board)
     for i from 0 upto (1- *dimension*)
     for column = (board-column board i)
     do
       (unless (list-solved? column target-sum)
	 (return nil))
     finally
       (return t)))

(defun diagonal (board)
  (loop
     with config = (configuration board)
     for i from 0 upto (1- *dimension*)
     collect (aref config i i) into diag
     finally (return diag)))

(defun cross-diagonal (board)
  (loop
     with config = (configuration board)
     for i from 0 upto (1- *dimension*)
     for index = (1- (- *dimension* i))
     collect (aref config index index) into diag
     finally (return diag)))

(defun both-diagonals-solved? (board)
  (let ((target-sum (target-sum board)))
    (when (list-solved? (diagonal board) target-sum)
      (list-solved? (cross-diagonal board) target-sum))))

(defun board-solved? (board)
  (block bail
    (let ((config (configuration board)))
      ;; look for NIL.  Its presence implies not-yet-solved.
      (dotimes (i *dimension*)
	(dotimes (j *dimension*)
	  (when (null (aref config i j))
	    (return-from bail nil))))
      (and (every-row-solved? board)
	   (every-column-solved? board)
	   (both-diagonals-solved? board)))))

(defun available-pieces (board)
  (let ((config (configuration board)))
    (let ((used-table (make-hash-table :test #'eql)))
      (dotimes (i *dimension*)
	(dotimes (j *dimension*)
	  (let ((piece (aref config i j)))
	     (when piece
	       (setf (gethash piece used-table) t)))))
      (loop
	 for i from 1 upto (* *dimension* *dimension*)
	 unless (gethash i used-table) collect i into unused
	 finally (return unused)))))

(defun copy-configuration (configuration)
  (let ((new-config (make-array (list *dimension* *dimension*) :initial-element nil)))
    (dotimes (i *dimension*)
      (dotimes (j *dimension*)
	(setf (aref new-config i j)
	      (aref configuration i j))))
    new-config))

(defun some-row-overflows? (board)
  (loop
     with target-sum = (target-sum board)
     for i from 0 upto (1- *dimension*)
     for row = (board-row board i)
     for numbers = (remove-if #'null row)
     for sum = (reduce #'+ numbers)
     do
       (when (> sum target-sum)
	 (return t))
     finally
       (return nil)))

(defun some-column-overflows? (board)
  (loop
     with target-sum = (target-sum board)
     for i from 0 upto (1- *dimension*)
     for column = (board-column board i)
     for numbers = (remove-if #'null column)
     for sum = (reduce #'+ numbers)
     do
       (when (> sum target-sum)
	 (return t))
     finally
       (return nil)))

(defun some-diagonal-overflows? (board)
  (let ((target-sum (target-sum board)))
    (let ((diag (diagonal board)))
      (let ((numbers-in-diag (remove-if #'null diag)))
	(if (<= (reduce #'+ numbers-in-diag) target-sum)
	    (let ((cross-diag (cross-diagonal board)))
	      (let ((numbers-in-cross-diag (remove-if #'null cross-diag)))
		(> (reduce #'+ numbers-in-cross-diag) target-sum)))
	    t)))))

(defun some-line-overflows? (board)
  (or (some-row-overflows? board)
      (some-column-overflows? board)
      (some-diagonal-overflows? board)))

(defun successors (board)
  (let ((available (available-pieces board))
	(succs nil)
	(config (configuration board))
	(target-sum (target-sum board)))
    (loop
       for piece in available
       do
	 (dotimes (i *dimension*)
	   (dotimes (j *dimension*)
	     (when (null (aref config i j))
	       (let ((new-config (copy-configuration config)))
		 (setf (aref new-config i j) piece)
		 (let ((new-board (make-instance 'board
						   :configuration new-config
						   :target-sum target-sum)))
		   (unless (some-line-overflows? board)
		     (push new-board succs))))))))
    succs))

(defun solve-puzzle (board)
  (if (board-solved? board)
      board
      (let ((succs (successors board)))
	(if succs
	    (loop
	       for succ in succs
	       for solution = (solve-puzzle succ)
	       do
		 (when solution
		   (unless (eq solution :fail)
		     (return solution)))
	       finally
		 (return :fail))
	    :fail))))
