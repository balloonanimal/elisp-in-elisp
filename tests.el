;; this file contains various different forms to test our reader against the builtin emacs reader

(+ 1 1)

(defun this-is-a-function (with some &rest args)
  "and a docstring"
  (declare (pure t))
  (let ((some-binding 10))
    (+ some-binding (length args))))
