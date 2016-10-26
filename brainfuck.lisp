;;;; brainfuck.lisp

(in-package #:brainfuck)

;;; "brainfuck" goes here. Hacks and glory await!

(defparameter *data-pointer* 0)

(defparameter *cell-array* (make-array 30000 :initial-element 0))

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
