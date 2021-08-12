;;; reader.el --- where the magic happens -*- lexical-binding: t; -*-

;; heavily inspired by clojure's tool.reader

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro comment (&rest body)
  "Ignores body, yields nil"
  nil)
(defun test- (op expected result)
  (if (funcall op expected result)
      :success
    (vector :fail expected result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [protocol] Reader
(cl-defgeneric reader-read-char (reader)
  "Returns the next char from the Reader, nil if the end of stream has been reached")

(cl-defgeneric reader-peek-char (reader)
  "Returns the next char from the Reader without removing it from the reader stream")

;; [protocol] PushbackReader
(cl-defgeneric reader-unread (pushback-reader)
  "Returns the next char from the Reader without removing it from the reader stream")

;; [type] StringReader
(cl-defstruct (StringReader (:constructor StringReader)
                            (:copier nil))
  s s-pos)

(cl-defmethod reader-peek-char ((reader StringReader))
  (let ((s (StringReader-s reader))
        (s-pos (StringReader-s-pos reader)))
    (when (> (length s) s-pos)
      (aref s s-pos))))

(cl-defmethod reader-read-char ((reader StringReader))
  (let ((c (reader-peek-char reader)))
    (cl-incf (StringReader-s-pos reader))
    c))

(cl-defmethod reader-unread ((reader StringReader))
  (cl-decf (StringReader-s-pos reader)))

(comment
 (setq sreader (StringReader :s "(hello world!" :s-pos 0))
 (test- #'= ?\( (reader-peek-char sreader))
 (test- #'= ?\( (reader-read-char sreader))
 (dotimes (i 5)
   (reader-read-char sreader))
 (test- #'= ?\ (reader-read-char sreader))
 (dotimes (i 10)
   (reader-read-char sreader))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Builtin Read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME: delegating to builtin read
(defun reader-builtin-read (s)
  (let ((out (read-from-string (StringReader-s s) (StringReader-s-pos s))))
    (setf (StringReader-s-pos s) (cdr out))
    (car out)))

(defun reader-builtin-read-from-string (s)
  (let ((out (read-from-string s)))
    (if (= (cdr out) (length s))
        (car out)
      (error "attempted to read incomplete form with builtin reader"))))

(comment
 (setq sreader (StringReader :s "hello world!" :s-pos 0))
 (reader-builtin-read sreader)
 (reader-builtin-read sreader)
 (reader-builtin-read-from-string "(+ 1 1)")
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reader-whitespace? (c)
  (memq c '(?\  ?\n ?\t)))

;; FIXME add more
(defun reader-macro-terminating? (c)
  (memq c '(?\" ?\; ?\( ?\[ ?\) ?\])))

(defun reader-ignore-until (reader end-c)
  (while (not (= end-c (reader-read-char reader)))))

(defun reader-digit-to-number (c base)
  (let ((digit (cond
                ((and (<= ?0 c) (<= c ?9)) (- c ?0))
                ((and (<= ?a c) (<= c ?z)) (+ (- c ?a) 10))
                ((and (<= ?A c) (<= c ?Z)) (+ (- c ?A) 10)))))
    (if (or (null digit)
            (>= digit base))
        nil
      digit)))

;; TODO
;; (defun reader-string-to-number (s base)
;;   (let* ((idx  0)
;;          (sign (cl-case (aref s idx)
;;                  (?\+ 1)
;;                  (?\- -1)))
;;          (idx (if ))
;;          (n    (let* ((d (reader-digit-to-number (aref s 0)))))))
;;     ;; TODO
;;     nil))

(comment
 (setq sreader (StringReader :s "hello world!" :s-pos 0))
 (reader-ignore-until sreader ?\ )
 (reader-builtin-read sreader)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reader-read-delimited (reader kind end-c)
  (let ((l '())
        (c (reader-peek-char reader)))
    (while (not (= c end-c))
      (let ((elt (reader-read* reader)))
        (push elt l))
      (setq c (reader-peek-char reader)))
    (reader-read-char reader)
    (nreverse l)))

(defun reader-read-list (reader c)
  (reader-read-delimited reader :list ?\)))

(defun reader-read-vector (reader c)
  (apply #'vector (reader-read-delimited reader :vector ?\])))

;; TODO figure out how emacs reads nums
;; (defun reader-read-num)

;; TODO figure out how emacs reads chars
(defun reader-read-char* (reader c)
  (reader-unread reader)
  (reader-builtin-read reader))

;; TODO figure out how emacs reads strings
(defun reader-read-string* (reader c)
  (reader-unread reader)
  (reader-builtin-read reader))

(defun reader-read-token (reader kind c)
  (if c
      (let ((l '())
            (c c))
        (while (not (or (reader-whitespace? c)
                        (reader-macro-terminating? c)
                        (null c)))
          (push c l)
          (setq c (reader-read-char reader)))
        (if c (reader-unread reader))
        (apply #'string (nreverse l)))
    (error "Unexpected EOF at start of reading %s" kind)))

;; FIXME
(defun reader-read-num-or-symbol (reader c)
  (reader-builtin-read-from-string (reader-read-token reader :num-or-symbol c)))

(defun reader-read-unmatched-delimiter (reader c)
  (error "Unmatched delimiter %s" (string c)))

(defun reader-read-dispatch (reader c)
  (reader-unread reader)
  (reader-builtin-read reader))

(defun reader-read-comment (reader c)
  (while (not (= (reader-read-char reader) ?\n)))
  reader)

(defun reader-read-quote (reader c)
  (reader-unread reader)
  (reader-builtin-read reader))

(defun reader-read-backquote (reader c)
  (reader-unread reader)
  (reader-builtin-read reader))


(defun reader-read-comma (reader c)
  (reader-unread reader)
  (reader-builtin-read reader))


(defun reader-read-period (reader c)
  (reader-unread reader)
  (reader-builtin-read reader))

(comment
 (setq sreader (StringReader :s "(1 2 3)" :s-pos 1))
 (reader-read-list sreader ?\()
 (setq sreader (StringReader :s "[1 2 3]" :s-pos 1))
 (reader-read-vector sreader ?\[)
 )


;; NOTE: reader macros are expected to return a reader object if additional
;; reading is required, for example reader-read-comment
(defun reader-macros (c)
  (cl-case c
    (?\( #'reader-read-list)
    (?\[ #'reader-read-vector)
    (?\) #'reader-read-unmatched-delimiter)
    (?\] #'reader-read-unmatched-delimiter)
    (?\# #'reader-read-dispatch)
    (?\; #'reader-read-comment)
    (?\' #'reader-read-quote)
    (?\` #'reader-read-backquote)
    (?\" #'reader-read-string*)
    (?\, #'reader-read-comma)
    (?\? #'reader-read-char*)
    ;; TODO: understand what this function does
    (?\. #'reader-read-period)
    (t   #'reader-read-num-or-symbol)))

;; read
(defun reader-read* (reader)
  (let ((read-result reader))
    (while (eq (type-of reader) (type-of read-result))
      (let ((c (reader-read-char reader)))
        (cond
         ((null c)               (signal 'end-of-file nil))
         ((reader-whitespace? c) nil)
         (t
          (setq read-result (funcall (reader-macros c) reader c))))))
    read-result))

(defun reader-read-string (s)
  (reader-read* (StringReader :s s :s-pos 0)))

(defun reader-read (stream)
  "Read one Lisp expression as text from STREAM, return as Lisp object.
If STREAM is nil, use the value of `standard-input' (which see).
STREAM or the value of `standard-input' may be:
 a buffer (read from point and advance it)
 a marker (read from where it points and advance it)
 a function (call it with no arguments for each character,
     call it with a char as argument to push a char back)
 a string (takes text from string, starting at the beginning)
 t (read text line using minibuffer and use it, or read from
    standard input in batch mode)."
  (let ((stream (or stream standard-input)))
    ;; TODO: handle stream ϵ {t, read_char}
    (reader-read* stream)))

(comment

 (defun reader--read-all-forms (s f)
   (let* ((reader (StringReader :s s :s-pos 0))
          (l '()))
     (while (reader-peek-char reader)
       (cond
        ((reader-whitespace? (reader-peek-char reader)) (reader-read-char reader))
        ((= (reader-peek-char reader) ?\;)              (progn (reader-read-char reader)
                                                               (reader-read-comment reader ?\;)))
        (t                                              (push (funcall f reader) l))))
     (nreverse l)))

 (defun reader--test-string (s)
   (let ((ours (reader--read-all-forms s #'reader-read*))
         (theirs (reader--read-all-forms s #'reader-builtin-read)))
     (if (equal ours theirs)
         :success
       (let ((b1 (generate-new-buffer "*ours*"))
             (b2 (generate-new-buffer "*theirs*")))
         (with-current-buffer b1 (insert (pp ours)))
         (with-current-buffer b2 (insert (pp theirs)))
         (ediff-buffers b1 b2)))))

 ;; TODO string matching doesn't remove comments
 (defun reader--test-file (filename)
   (let* ((s (with-temp-buffer
               (insert-file-contents filename)
               (buffer-string))))
     (reader--test-string s)))


 (reader--test-string "1")
 (reader--test-string "(+ 1 1)")
 (reader--test-string "[+ 1 1]")
 (reader--test-string "; comment\n(+ 1 1)")
 (reader--test-string "?a")
 (reader--test-string "nil")

 (reader-read-string ")")
 (reader-read-string "(+ 1 1")


 (reader--test-file "../tests.el")
 (reader--test-file "/home/zach/.emacs.d/init.el")
 (reader--test-file "/home/zach/.emacs.d/core/core.el")
 (reader--test-file "/home/zach/.emacs.d/core/core-ui.el")
 (reader--test-file "/home/zach/.emacs.d/.local/straight/repos/org/lisp/org.el")


 ;; TODO doesn't work, somethign about defun inside a macro and the obarray
 (defmacro log-function! (functions)
   (let* ((functions (if (listp functions) functions (list functions)))
          (advice-bodies
           (mapcar
            (lambda (target)
              `(defun ,(intern (concat (symbol-name target) "--log-advice")) (,target &rest args)
                 (let ((form (cons ,(concat  "" (symbol-name target)) args))
                       (output (apply ,target args)))
                   (message "[INFO] %s\n[INFO] ;; => %s" form output)
                   output)))
            functions))
          (add-advices
           (mapcar
            (lambda (target)
              `(advice-add ',target :around #',(intern (concat (symbol-name target) "--log-advice"))))
            functions)))
     `(progn ,@advice-bodies ,@add-advices)))

 (defmacro unlog-function! (functions)
   (let* ((functions (if (listp functions) functions (list functions)))
          (remove-advices
           (mapcar
            (lambda (target)
              `(advice-remove ',target #',(intern (concat (symbol-name target) "--log-advice"))))
            functions)))
     `(progn ,@remove-advices)))


 ;; (log-function! reader-read-num-or-symbol)
 (log-function! reader-read*)
 (log-function! reader-read-list)
 (log-function! reader-macros)
 (unlog-function! reader-read-list)
 (unlog-function! reader-read*)
 )
