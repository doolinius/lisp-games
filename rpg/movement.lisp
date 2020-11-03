(defun not-on-map (coord map-size)
  (or (< (car coord) 0)
      (< (cdr coord) 0)
      (> (car coord) (car map-size))
      (> (cdr coord) (cadr map-size))))

(defun adjacent-coords (square)
  (let ((x (car square))
	(y (cdr square)))
    (list (cons (1+ x) y)
	  (cons (1- x) y)
	  (cons x (1+ y))
	  (cons x (1- y)))))

(defun get-te-map (gchar map)
  ())

;;; adjacent - returns adjacent walkable coordinates according to map dimensions
;;;            and terrain effects
;;;          coord - coordinate for which we are finding adjacent walkable coords
;;;          map
(defun adjacent (coord map)
  (remove-if (lambda (c)
	       (not-on-map c (array-dimensions map)))
	     (adjacent-coords coord)))

;;; c1.x, c1.y : c2.x, c2.y
;;; (c1.x - c2.x) + (c1.y - c2.y)
;;;
;;; example: (10, 5), (10, 10) = (+ 0 5) = 5
;;; 	     (0, 0), (3, 3) = (+ 3 3) = 6
(defun coord-distance (c1 c2)
  (+ (abs (- (car c1) (car c2)))
     (abs (- (cdr c1) (cdr c2)))))

;;; make a map
(defparameter *map* (make-array '(10 10) :initial-element :grass))

;;; brush terrain
(loop for i from 0 to 9
   do (loop for k from 4 to 9
	   do (setf (aref *map* i k) :brush)))

;;; make a few things solid
(setf (aref *map* 3 2) :solid)

;;; terrain effect values (decrement)
(defparameter *te* (list :grass 1.0 :brush 1.5 :solid 20))

;;; get the mov decrement value for a particular tile
(defun mov-dec (coord)
  (getf *te* (aref *map* (car coord) (cdr coord))))

;;; coord is current player coordinate
;;; mov is their movement stat
;;; map is the current tilemap

;;; returns a list of coordinates the character can
;;; move to
(defun move-area (coord mov map)
  (labels ((next-steps (nc mov)
	     (if (< mov (mov-dec nc))
		 nil
		 (append (list nc)
			 (mapcan (lambda (c)
				   (next-steps c (- mov (mov-dec nc))))
				 (adjacent nc map))))))
    (remove-duplicates (append (list coord)
			       (mapcan (lambda (c)
					 (next-steps c mov))
				       (adjacent coord map))) :test #'equal)))
 
(defun show-mov-area (ma map)
  (let ((newmap (make-array (array-dimensions map) :initial-element 0)))
    (progn
      (mapc (lambda (c)
	    (setf (aref newmap (car c) (cdr c)) 1))
	    ma)
      newmap)))

(defstruct node
  (coord)
  (parent)
  (f)
  (g)
  (h))

(defun min-f (nodes)
  (labels ((get-min (m lst)
	     (cond ((null lst) m)
		   ((< (node-f (car lst)) (node-f m)) (get-min (car lst) (cdr lst)))
		   (t (get-min m (cdr lst))))))
    (get-min (car nodes) (cdr nodes))))

(defun geometric-distance (coord1 coord2)
  (let ((a (abs (- (car coord1) (car coord2))))
	(b (abs (- (cdr coord1) (cdr coord2)))))
    (sqrt (+ (expt a 2) (expt b 2)))))

(defun heuristic-distance (node1 node2 &optional (type 1))
  (cond ((= type 1)
	 (coord-distance (node-coord node1) (node-coord node2)))
	((= type 2)
	 (geometric-distance (node-coord node1) (node-coord node2)))))

(defun a* (src dst)
  (let ((goal (make-node :coord dst))
	(open (list (make-node :coord src :f 0 :g 0)))
	(closed '()))
    (do ((found nil))
	((or found (null open)) closed)
      (let ((q (min-f open)))
	(progn
	  (format t "~d items in OPEN~%" (length open))
	  (format t "~A~%" open)
	  (format t "~d items in CLOSED~%" (length closed))
	  (format t "~A~%" closed)
	  (format t "q is set, COORD: ~d, ~d...~%" (car (node-coord q))
		  (cdr (node-coord q)))
	  (setf open (remove q open))
	  (dolist (successor (mapcar (lambda (c)
				       (make-node :coord c :parent (node-coord q)))
				     (adjacent (node-coord q) *map*)))
	    (if (equal (node-coord successor) (node-coord goal))
		(setf found t)
		(progn
		  (format t "inspecting successor: ~A~%" successor)
		  (setf (node-g successor) (1+ (node-g q)))
		  (setf (node-h successor) (heuristic-distance successor goal))
		  (setf (node-f successor) (+ (node-g successor) (node-h successor)))
		  (unless
		      (find-if (lambda (n)
				 (and (equal (node-coord n) (node-coord successor))
				      (= (node-f n) (node-f successor))))
			       (append open closed))
		    (push successor open)))))
	  (format t "Press Enter to continue...~A~%" (read))
	  (push q closed))))))
	   
;(defun find-path (character coord map)
