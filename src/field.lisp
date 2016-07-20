(in-package :botclean)

(defstruct pos
  x
  y)

(defclass field ()
  ((width :reader field-width
	  :initarg :width)
   (height :reader field-height
	   :initarg :height)
   (cells :initform nil
	  :initarg :cells
	  :accessor field-cells)))

(defun get-cell (field pos)
  (with-slots (field-cells) field
    (aref field-cells (pos-y pos) (pos-x pos))))

;;mutable
(defun put-cell (field pos val)
  (with-slots (field-cells) field
    (setf (aref field-cells (pos-y pos) (pos-x pos))
          val)))

(defclass state ()
  ((field :accessor state-field
	  :initarg :field)
   (bot-pos :accessor state-bot-pos
	    :initarg :bot-pos)
   (moves-count :accessor state-moves-count
		:initarg :moves-count
		:initform 0)
   (dirty-cells :accessor state-dirty-cells
		:initarg :dirty-cells)))

(defun parse-field (str)
  (with-input-from-string (stream str)
    (let* ((lines (loop for line = (read-line stream nil)
		     while line
		     collect line))
	   (height (length lines))
	   (width (length (first lines)))
	   (field (make-instance 'field :width width :height height))
	   (bot-pos)
	   (dirty-cells))
      (loop for line in lines
	 for y from 0 do
	   (loop for ch across line
	      for x from 0 do
		(case ch
		  (#\b (setf bot-pos (make-pos :x x :y y)))
		  (#\d (push (make-pos :x x :y y) dirty-cells)
		       (put-cell field (make-pos :x x :y y) :dirt))
		  (#\# (put-cell field (make-pos :x x :y y) :wall)))))
      (make-instance 'state
		     :field field
		     :bot-pos bot-pos
		     :dirty-cells dirty-cells))))

(defun possible-moves (pos width height)
  (append
   (when (> (pos-x pos) 0)
     (list :left))
   (when (> (pos-y pos) 0)
     (list :up))
   (when (< (pos-x pos) (1- width))
     (list :right))
   (when (< (pos-y pos) (1- height))
     (list :down))))
