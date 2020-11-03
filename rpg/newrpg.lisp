(defpackage :com.ilextronics.rpg
  (:use :common-lisp))

(in-package :com.ilextronics.rpg)

(defstruct stat
  value
  progression
  param)

(defgeneric update-stat (st level))

(defmethod update-stat ((st stat) level)
  (incf (stat-value st) (funcall (stat-progression st) level (stat-param st))))

(defun make-stat-list (stats)
  (if (null stats)
      nil
      (cons (cons (car stats) (make-stat)) (make-stat-list (cdr stats)))))

(defparameter *stat-list* '(strength defense intelligence agility luck critical))



(defstruct gcharacter
  name
  level
  exp-pts
  char-class
  (max-hp (make-stat))
  hp
  (max-mp (make-stat))
  mp
  (stats (make-stat-list *stat-list*))
  move 
  weapon
  ring
  accessory
  items
  spells
  statuses
  resistance
  kills
  defeats
  extra)

(defgeneric strength (obj))

(defmethod strength ((obj gchar))
  (stat-value (car (assoc 'strength (gchar-stats obj)))))

(defmethod (setf strength) (value (obj gchar))
  (setf
   (stat-value (car (assoc 'strength (gchar-stats obj))))
   value))

(defmacro defstat (name game-object)
  (let ((stat-func (intern (format nil "~A-STATS" game-object))))
    `(progn
       (defgeneric ,name (obj))
       (defmethod ,name ((obj ,game-object))
	 (stat-value (car (assoc (quote ,name) (,stat-func obj)))))
       (defmethod (setf ,name) (value (obj ,game-object))
	 (setf
	  (stat-value (car (assoc (quote ,name) (,stat-func obj))))
	  value)))))
					      
(defmacro make-stats (&rest stats)
  `(progn
     ,@(loop for stat in stats
	  collecting
	    `(progn
	       (defstat ,stat gchar)
	       (defstat ,stat weapon)
	       (defstat ,stat ring)
	       (defstat ,stat char-class)
	       (defun ,(intern (format nil "GCHAR-~A" stat)) (gchar)
		 (+
		  (,stat (gchar-weapon gchar))
		  (,stat (gchar-ring gchar))
		  (,stat (gchar-char-class gchar))
		  (,stat gchar)))))))
