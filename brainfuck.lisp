;;;; brainfuck.lisp

(defpackage #:brainfuck
  (:use #:cl))

(in-package #:brainfuck)

;;; "brainfuck" goes here. Hacks and glory await!

(defconstant *default-data-pointer* (quote 0))
(defconstant *default-cell-array* (quote (make-array 30000 :initial-element 0)))
(defconstant *default-while-stack* (quote nil))

(defparameter *data-pointer* (eval *default-data-pointer*))
(defparameter *cell-array* (eval *default-cell-array*))
(defparameter *while-stack* (eval *default-while-stack*))

(defmacro current-cell ()
  (aref *cell-array* *data-pointer*))

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
  (setf (current-byte) (read-byte *standard-input*)))

(defun jump-forwards ()
  )

(defun jump-backwards ()
  )

(defun restart-env ()
  (setf *data-pointer* (eval *default-data-pointer*))
  (setf *cell-array* (eval *default-cell-array*))
  (setf *while-stack* (eval *default-while-stack*)))

(defun parse-bf (str)
  (do ((string-pos 0 (1+ string-pos)))
      (= string-pos (length str))
      (ecase command
        (#\+ (increment-cell))
        (#\- (decrease-cell))
        (#\> (advance-pointer))
        (#\< (back-pointer))
        (#\. (output-byte))
        (#\, (input-byte)))))
