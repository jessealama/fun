(defun contains-duplicates? (configuration)
  (if configuration
      (let ((first (first configuration))
	    (rest (rest configuration)))
	(if (member first rest :test #'=)
	    t
	    (contains-duplicates? rest)))
      nil))

(defun possibilities (target num-squares)
  (if (< target 1)
      nil
      (if (< num-squares 1)
	  nil
	  (if (= num-squares 1)
	      (if (< target 10)
		  (list (list target))
		  nil)
	      (loop
		 with more = nil
		 for i from 1 upto 9
		 do
		   (let ((smaller (possibilities (- target i)
						 (1- num-squares))))
		     (let ((candidates (mapcar #'(lambda (possibility)
						   (append (list i)
							   possibility))
					       smaller)))
		       ;; (break "candidates = ~a" candidates)
		       (let ((trimmed (remove-if #'contains-duplicates?
						 candidates)))
			 (setf more (append more trimmed)))))
		 finally
		   (setf more (mapcar #'(lambda (possibility)
					  (sort possibility #'<))
				      more))
		   (setf more (remove-duplicates more :test #'equalp))
		   (return more))))))
