(defmacro contains (x n)
  `(member ,n ,x :test #'=))

(defun contains-1? (x)
  (contains x 1))

(defun contains-2? (x)
  (contains x 2))

(defun contains-3? (x)
  (contains x 3))

(defun contains-4? (x)
  (contains x 4))

(defun contains-5? (x)
  (contains x 5))

(defun contains-6? (x)
  (contains x 6))

(defun contains-7? (x)
  (contains x 7))

(defun contains-8? (x)
  (contains x 8))

(defun contains-9? (x)
  (contains x 9))

(defun contains-duplicates? (configuration)
  (when configuration
    (let ((first (first configuration))
	  (rest (rest configuration)))
      (or (member first rest :test #'=)
	  (contains-duplicates? rest)))))

(defun sort-< (list)
  (let ((sorted (sort list #'<)))
    sorted))

(defun possibilities (target num-squares)
  (cond ((< target 1)
	 nil)
	((< num-squares 1)
	 nil)
	((= num-squares 1)
	 (when (< target 10)
	   (list (list target))))
	(t
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
		  (let ((trimmed (remove-if #'contains-duplicates?
					    candidates)))
		    (setf more (append more trimmed)))))
	    finally
	      (return (remove-duplicates (mapcar #'sort-< more)
					 :test #'equalp))))))
