(level-up *lerris*)

(stat-progressions *lerris*)

(list
 :strength (#'linear 0.75)
 :maxHP (#'linear 1.0)
 :maxMP (#'linear 0)
 :intelligence (#'slow-fast 3)
 :agility (#'fast-slow 5))

;;; generates a list of possible stat increases
;;; chooses one at random.  distribution of increases
;;; determines likelihood of stat being chosen
(defun get-stat-increase (progression-func factor level)
  (let* ((dist (funcall progression-func factor level)))
    (nth (random (length dist)) dist)))

(defun linear (factor level)
  ())
    

