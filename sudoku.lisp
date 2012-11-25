(defun contains-duplicates? (configuration)
  (when configuration
    (let ((first (first configuration))
	  (rest (rest configuration)))
      (or (member first rest :test #'=)
	  (contains-duplicates? rest)))))

(defun sort-< (list)
  (sort list #'<))

(defun possibilities (target num-squares)
  (cond ((< target 1) nil)
	((< num-squares 1) nil)
	((= num-squares 1) (when (< target 10) (list (list target))))
	(t
	 (loop
	    with more = nil
	    for i from 1 upto 9
	    for smaller = (possibilities (- target i) (1- num-squares))
	    for candidates = (mapcar #'(lambda (possibility)
					 (append (list i)
						 possibility))
				     smaller)
	    do
	      (setf more (append more (remove-if #'contains-duplicates?
						 candidates)))
	    finally
	      (return (remove-duplicates (mapcar #'sort-< more)
					 :test #'equalp))))))
