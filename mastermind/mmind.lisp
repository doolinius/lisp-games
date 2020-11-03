(defun my-command-line ()
  (or 
   #+SBCL *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   #+CLISP *args*
   nil))

(defparameter *colors* '(red blue yellow green brown purple))

(setf *random-state* (make-random-state t))

(defvar *code*)

(defun make-code (size color-list)
  (if (eq 0 size)
      nil
      (cons 
       (nth (random (length color-list)) color-list) 
       (make-code (- size 1) color-list))))

(defun check-exact (guess code)
  (if (eq guess nil)
      nil
      (if (eq (car guess) (car code))
	  (cons 'black (check-exact (cdr guess) (cdr code)))
	  (cons (car guess) (check-exact (cdr guess) (cdr code))))))

(defun filter-code (exacts code)
  (mapcar #'(lambda (x y) (if (eq 'black x) nil y)) exacts code))

(defun remove-once (item list)
  (if (eq item (car list))
      (cdr list)
      (cons (car list) (remove-once item (cdr list)))))

(defun count-intersection (list1 list2)
  (if list1
      (let ((item (car list1)))
	(if (find item list2)
	    (cons item (count-intersection (cdr list1) (remove-once item list2)))
	    (count-intersection (cdr list1) list2)))
      nil))

(defun check-inexact (exacts code)
  (let ((result (count-intersection (filter-code exacts code) exacts)))
    (coerce (make-array (length result) :initial-element 'white) 'list)))

(define-condition incorrect-code-size-error (error)
  ((code-guess :initarg :code-guess :reader code-guess)))

(defun check-guess (guess code)
  (if (= (length guess) (length code))
      (let ((exacts (check-exact guess code)))
	(append (remove-if-not #'(lambda (color) (eq color 'black)) exacts)
		(check-inexact exacts code)))))
(defun prompt-read (prompt)
  (format *query-io* "~a" prompt)
  (force-output *query-io*)
  (read-from-string (concatenate 'string "(" (read-line *query-io*) ")")))

(defun successp (code-size result)
  (= code-size (count 'black result)))

(defun make-guess (try-number code)
  (let ((result (check-guess (prompt-read (format nil "Enter Guess [~A]: " try-number)) code)))
    (progn
      (if result (format t "~{~A ~}~%" result))
      (successp (length code) result))))

(defun print-intro (code-size number-of-tries colors)
  (progn
    (format t "Welcome to CodeBreaker!~%~%")
    (format t "~TBreak the code by guessing the sequence of colors~%")
    (format t "~TChoose any sequence of ~:@(~r~) of the following colors:~%" code-size)
    (format t "~T~T~{~A ~}~%" colors)
    (format t "~TI will tell you the results of your guess with colors of my own~%")
    (format t "~TBlack means you guessed a color and it's in the right place~%")
    (format t "~TWhite means you guessed a color but it's not in the right place~%")
    (format t "~TExample: say the hidden code is RED BLUE RED YELLOW~%")
    (format t "~TYou make your guess by typing: brown yellow red brown~%")
    (format t "~TThe results will be: BLACK WHITE~%")
    (format t "~TBecause one of the colors you guessed (red) is right AND in the right place~%")
    (format t "~Tand another (yellow) is right, but not in the right place~%~%")
    (format t "~TYou have ~:@(~r~) tries to break the code.  Good luck!~%~%" number-of-tries)))

(defun main (number-of-tries size-of-code)
  (progn
    (print-intro size-of-code number-of-tries *colors*)
    (let ((answer T))
      (loop
	 (when (not answer)
	   (quit))
	 (let ((success nil) (current-code (make-code size-of-code *colors*)))
	   (do ((n 0 (1+ n)))
	       ((or (= number-of-tries n) success) (if success (format t "You broke the code!~%") (format t "You didn't break the code: ~{~A ~}.~%Better luck next time.~%" current-code)))
	     (setf success (make-guess (1+ n) current-code))))
	 (setf answer (yes-or-no-p "Would you like to try again?"))))))
  
(main 12 4)
