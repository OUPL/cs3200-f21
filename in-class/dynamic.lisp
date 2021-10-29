(defvar x)

(defun f ()
  (let ((x 123))
    (lambda () (format t "~d" x))))

(defun h (g)
  (let ((x 5))
    (funcall g)))

(let ((g (f)))
  (h g))
