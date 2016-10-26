;;;; brainfuck.lisp

(defpackage #:brainfuck
  (:use #:cl))

(in-package #:brainfuck)

;;; "brainfuck" goes here. Hacks and glory await!

(defconstant +DEFAULT-DATA-POINTER+ (quote 0))
(defconstant +DEFAULT-CELL-ARRAY+ (quote (make-array 30000 :initial-element 0)))
(defconstant +DEFAULT-WHILE-STACK+ (quote nil))

(defparameter *data-pointer* (eval +DEFAULT-DATA-POINTER+))
(defparameter *cell-array* (eval +DEFAULT-CELL-ARRAY+))
(defparameter *while-stack* (eval +DEFAULT-WHILE-STACK+))

(defmacro current-cell ()
  `(aref *cell-array* *data-pointer*))

(defun advance-pointer ()
  (if (< *data-pointer* (length *cell-array*))
      (incf *data-pointer*)
      (error "No more cells!")))

(defun back-pointer ()
  (if (zerop *data-pointer*)
      (error "No more cells!")
      (decf *data-pointer*)))

(defun increment-cell ()
  (incf (current-cell)))

(defun decrement-cell ()
  (decf (current-cell)))

(defun output-byte ()
  (format t "~A" (code-char (current-cell))))

(defun input-byte ()
  (setf (aref *cell-array* *data-pointer*) (read-byte *standard-input*)))

(defun jump-forwards ()
  )

(defun jump-backwards ()
  )

(defun restart-env ()
  (setf *data-pointer* (eval *default-data-pointer*))
  (setf *cell-array* (eval *default-cell-array*))
  (setf *while-stack* (eval *default-while-stack*)))

(defun parse-bf (str)
  (restart-env)
  (do ((string-pos 0 (1+ string-pos)))
       ((= string-pos (length str)))
    (ecase (aref str string-pos)
        (#\+ (increment-cell))
        (#\- (decrement-cell))
        (#\> (advance-pointer))
        (#\< (back-pointer))
        (#\. (output-byte))
        (#\, (input-byte)))))
