(defvar *alpha* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(defun readfile (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning the string"
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun prepmessage (msg)
  (mapcar 'intern (mapcar 'string (coerce (string-upcase (remove-if-not #'alpha-char-p msg)) 'list))))

(defun make-lookup (keyword letter)
  (let ((new-alpha)
	(pos (- 26 (position letter *alpha*))))
    (setf new-alpha (remove-duplicates (append keyword *alpha*) :from-end t))
    (append (subseq new-alpha pos) (subseq new-alpha 0 pos))))

(defun replace-letter (letter alpha1 alpha2)
  (let ((pos (position letter alpha1)))
    (nth pos alpha2)))

(defun encrypt (keyword letter msg)
  (let ((lookup (make-lookup keyword letter)))
    (eval (append '(concatenate 'string) (mapcar 'symbol-name (mapcar #'(lambda (c) (replace-letter c *alpha* lookup)) msg))))))

(defun decrypt (keyword letter msg)
  (let ((lookup (make-lookup keyword letter)))
    (eval (append '(concatenate 'string) (mapcar 'symbol-name (mapcar #'(lambda (c) (replace-letter c lookup *alpha*)) msg))))))

(defun main ()
  (let ((o (nth 1 sb-ext:*posix-argv*))
	(k (remove-duplicates (prepmessage (nth 2 sb-ext:*posix-argv*)) :from-end t))
	(l (intern (string-upcase (nth 3 sb-ext:*posix-argv*))))
	(m (prepmessage (readfile (nth 4 sb-ext:*posix-argv*)))))
		
    (format t "~&~a~&" (cond ((equal o "e") (encrypt k l m))
			     ((equal o "d") (decrypt k l m))))
    (sb-ext:quit)))
