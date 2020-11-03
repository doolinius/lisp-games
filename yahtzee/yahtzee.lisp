(defvar *dice*)
(defvar *players*)
(defvar *rules*)
(defvar *top-section*)
(defvar *current-player*)
(defvar *selected-game* nil)
(defparameter *max-rolls* 3)
(defparameter *roll-num* 1)
(defparameter *winning-player* nil)
(defparameter *top-slots* '(ones 1 twos 2 threes 3 fours 4 fives 5 sixes 6))

(setf *random-state* (make-random-state t))

;;; creates a function that simply takes a body
;;; this body will check the results of scoring
;;; functions and return the actual scores for the particular game
;;; the body of defscore allows complex calculations
(defmacro defscore (&body body)
  `(lambda (arg)
     (declare (ignorable arg))
     ,@body))

;;; SCORING FUNCTIONS - checks dice requirements and returns 
;;;                     information useful for scoring or NIL

;;; loop from highest to lowest so that if there are two pair
;;; returns the number that makes up the pair
;;; the function will return the higher value if there are two
(defun one-pair (dice)
  (loop for x
       from 6
       downto 1
       do
       (when (<= 2 (count x dice))
	 (return-from one-pair x))))

;;; checks for two pair
;;; first checks for one pair, then removes those two
;;; then checks for a second pair.  Works for 4 of a kind.
(defun two-pair (dice)
  (let* ((p1 (one-pair dice))
	 (p2 (one-pair (remove p1 dice :count 2))))
    (when (and p1 p2)
      (list p1 p2))))

;;; NORMAL YAHTZEE STYLE FUNCTIONS

;;; the typical top section scoring algorithm
;;; returns the value being scored times the number rolled
(defun score-top-die (n dice)
  (* n (count n dice)))

;;; totals all dice
(defun total-dice (dice)
  (reduce #'+ dice))

;;; checks to see if the dice contain an arbitrary number
;;; of any one value.  eg., 3, 4, 5 of a kind
;;; returns the value.  For example, 5 5 5 4 1 would return
;;; 5, as there were three fives found
(defun is-n-of-a-kind (n dice)
  (dotimes (x 6)
    (when (<= n (count (1+ x) dice)) 
	(return-from is-n-of-a-kind (1+ x)))))

;;; uses is-n-of-a-kind to check for 3 of a kind
;;; returns the value found
(defun is-three-of-a-kind (dice)
  (is-n-of-a-kind 3 dice))

;;; uses is-n-of-a-kind to check for 4 of a kind
;;; see above
(defun is-four-of-a-kind (dice)
  (is-n-of-a-kind 4 dice))

;;; checks for 5 of a kind.  I could have used is-n-of-a-kind
;;; but recursion and lambda is more fun
(defun is-yahtzee (dice)
  (let ((first-die (car dice)))
    (when (every (lambda (die) (equal die first-die)) (cdr dice))
      first-die)))

;;; checks for Full House: a three of a kind and a pair
;;; this is merely returning T or NIL.  Should eventually
;;; return the two values comprising the Full House
;;; nothing needs it right now but it still doesn't conform
(defun is-full-house (dice)
  (let ((dice (sort (copy-list dice) #'<)))
    (or
     (and
      (eq (first dice) (second dice))
      (eq (third dice) (fourth dice))
      (eq (fourth dice) (fifth dice)))
     (and
      (eq (first dice) (second dice))
      (eq (second dice) (third dice))
      (eq (fourth dice) (fifth dice))))))

;;; checks to see if any number of dice are a run, such as 1,2,3,4
;;; or 2,3,4,5,6.
;;; Returns the lowest value found in the run
(defun is-straight (dice)
  (let ((sorted-dice (sort (copy-list dice) #'<)))
    (labels ((is-series (dice)
	       (if (equal 1 (length dice))
		   T
		   (if (eq (1+ (car sorted-dice)) (cadr sorted-dice))
		       (is-straight (cdr sorted-dice))
		       NIL))))
      (when (is-series sorted-dice)
	(car sorted-dice)))))

;;; checks for a run of five that starts with 1.  Called Little Straight
;;; or Small Straight in some variations.  returns T or NIL right now
(defun is-low-straight (dice)
  (equal 1 (is-straight dice)))

;;; checks for a run of five that starts with 2.  Called Big Straight
;;; or Large Straight in some variations.  returns T or NIL
(defun is-high-straight (dice)
  (equal 2 (is-straight dice)))

;;; just an alias for "is-straight", intended only to be used with 5 dice
(defun is-large-straight (dice)
  (is-straight dice))

;;; checks for a run of four by using is-straight on either the first 4
;;; or last 4 dice.  It will return the low value of the run of 4
(defun is-small-straight (dice)
  (let ((unique-dice (remove-duplicates dice)))
    (if (>= (length unique-dice) 4)
	(is-straight unique-dice)
	nil)))

;;; checks to see if any list contains all unique numerical
;;; elements (or any element that can be compared with < (less than)
;;; This will be used for determining a Cheerio straight with Wild Ones
;;; and a Generala wrap-around straight
(defun unique (lst)
  (let ((newlist (sort (copy-list lst) #'<)))
    (labels ((u (lst)
	       (if (null (cdr lst))
		   T
		   (and (< (car lst) (cadr lst))
			(u (cdr lst))))))
      (u newlist))))

;;; Generala allows wrap-around straights as an option
;;; this function basically checks if each die is unique
;;; as that is functionally the same as a straight of any kind,
;;; even wrap-around
(defun is-generala-straight (dice)
  (unique dice))

;;; CHEERIO FUNCTIONS

;;; Top die score if using Wild Ones in Cheerio
(defun score-top-die-wilds (n dice)
  (if (equal n (is-yahtzee dice))
      (* 10 n)
      (score-top-die n 
		     (if (member n dice)
			 (substitute n 1 dice)
			 dice))))

;;; check for a traditional cheerio and return dishonest (1) or honest (!1)
(defun is-cheerio (dice)
  (cond ((honest-cheerio dice) (apply #'max dice))
	((dishonest-cheerio dice) 1)
	(t nil)))

;;; check for a "cheerio" (straight or yahtzee) with no wilds
(defun honest-cheerio (dice)
  (or (is-yahtzee dice)
      (is-straight dice)))

;;; check for a "cheerio" with ones wild
(defun dishonest-cheerio (dice)
  (or (cheerio-yahtzee-wilds dice)
      (cheerio-straight-wilds dice)))

;;; check for a cheerio yahtzee with ones wild
(defun cheerio-yahtzee-wilds (dice)
  (and (some (lambda (x)
	       (= 1 x))
	     dice)
       (is-yahtzee (remove 1 dice))))

;;; check for a cheerio straight with ones wild
(defun cheerio-straight-wilds (dice)
  (unique (remove 1 dice)))

;;; KISMET FUNCTIONS    

;;; colors of the kismet dice according to number
(defparameter *kismet-colors*
  (make-array 6 :initial-contents '(black red green green red black)))

;;; returns a list of corresponding colors for a set of dice
(defun dice-colors (dice)
  (mapcar (lambda (die)
	    (aref *kismet-colors* (1- die)))
	  dice))

;;; checks to see if two dice have the same color
(defun same-color (die1 die2)
  (eq (aref *kismet-colors* (1- die1)) (aref *kismet-colors* (1- die2))))

;;; returns  T or NIL until is-full-house returns the two values
;;; that make up the full house
(defun is-full-house-same-color (dice)
  (when (and (is-flush dice) (is-full-house dice))
    (is-full-house dice)))

;;; checks to see if there are two pair of the same color
;;; returns the values of the two pair
(defun two-pair-same-color (dice)
  (let ((tp (two-pair dice)))
    (when (and tp (apply #'same-color tp))
      tp)))

;;; checks for all the same color
;;; returns the color if true
(defun is-flush (dice)
  (let* ((colors (dice-colors dice))
	 (first-color (car colors)))
    (when (every (lambda (color) (eq color first-color)) (cdr colors))
      first-color)))

;;; TOP SECTION FUNCTIONS
;;;     the top section in most yahtzee games is exactly the same
;;;     this function creates a top-section slot in the same format
;;;     as the ygame bottom-section rules
;;;     (name-string, function used for checking validity, function used to calculate score
;;;     Old Cheerio is an exception, where Ones are Wild

;;; creates a single top section slot
(defun make-top-section-slot (name number game)
  (let ((name-string (string-capitalize (symbol-name name))))
    (list (intern (string-upcase name-string) "KEYWORD")
	  (if (eq game :old-cheerio)
	      `(,name-string ,#'identity ,(defscore (score-top-die-wilds number *dice*)))
	      `(,name-string ,#'identity ,(defscore (score-top-die number *dice*)))))))

;;; returns a list of top section slots
(defun make-top-section (slots game)
  (if (null slots)
      nil
      (append (make-top-section-slot (car slots) (cadr slots) game)
	      (make-top-section (cddr slots) game))))

;;; structure defining a Yahtzee variations
(defstruct ygame
  (name)              ; name of the variation
  (short-description) ; short description used in main menu
  (long-description)  ; long description that can be viewed from main menu
  (rules)             ; the bottom section rules for the variation
  (top-bonus)         ; function to calculate top bonus, if applicable
  (y-bonus)           ; function to calculate 5 of a kind bonuses, if applicable
  (options))          ; options for the variation (such as scoring values)

;;; counts the number of bottom section rules, adds six for top section
;;; this will be used to count the number of rounds
(defun ygame-rounds (ygame)
  (/ (length (ygame-rules ygame)) 2))

;;; gets a game option of a given name, such as :top-section-bonus
(defun game-option (option)
  (getf (getf *selected-game* :options) option))

;;; creates a property list of games.  Each key is a symbol, each value
;;; is a ygame struct.  This is where the bulk of each game variation's
;;; rules are defined.  Edit this to add a new variation
(defparameter *games*
  (list :yahtzee
	(make-ygame :name "Yahtzee"
		    :short-description "the Hasbro classic dating back to 1954."
		    :long-description "Top Section - multiply the value of the die by the number of that die rolled.~%Top Bonus: 35 points if Top Section score is 63 or greater.~%Bottom Section: ~%3 of a Kind: total of all dice~%4 of a Kind: total of all dice~%Full House: three of a kind and a pair, 25 points~%Small Straight: a run of four, 30 points~%Large Straight: a run of five, 40 points~%Yahtzee: 5 of a kind, 50 points~%Chance: no requirements, total of all dice~%Yahtzee Bonus: after scoring a Yahtzee, all subsequent Yahtzees will be scored in other slots as appropriate, but you will receive an additional 100 points per Yahtzee.  This does not apply if you have already taken a 0 (zero) in the Yahtzee slot~%"
		    :top-bonus (lambda (x) (if (>= x 63) 35 0))
		    :y-bonus `("Yahtzee Bonus" :yahtzee ,(lambda (ysheet)
							(incf (getf (ysheet-score-fields ysheet) :y-bonus) 100)))
		    :options '(:y-bonus-p ("Use Yahtzee Bonus." t nil))
		    :rules `(:three-of-a-kind ("Three of a Kind" ,#'is-three-of-a-kind ,(defscore (total-dice *dice*)))
			    :four-of-a-kind ("Four of a Kind" ,#'is-four-of-a-kind ,(defscore (total-dice *dice*)))
			    :full-house ("Full House" ,#'is-full-house ,(defscore 25))
			    :small-straight ("Small Straight" ,#'is-small-straight ,(defscore 30))
			    :large-straight ("Large Straight" ,#'is-large-straight ,(defscore 40))
			    :yahtzee ("Yahtzee" ,#'is-yahtzee ,(defscore 50))
			    :chance ("Chance" ,#'identity ,(defscore (total-dice *dice*)))))

	:yacht
	(make-ygame :name "Yacht"
		    :short-description "The \"Yacht\" game and the father of Yahtzee."
		    :long-description "The \"Yacht\" game was invented by a Canadian couple who developed it for playing on their yacht with friends.  Its popularity spread and eventually Hasbro, inspired by Yacht, developed Yahtzee.~%~%Top Section: (same as Yahtzee)~%Top Bonus: Yacht has no top bonus~%Bottom Section:~%Little Straight: (1,2,3,4,5), 30 points~%Big Straight: (2,3,4,5,6), 30 points~%Full House: 3 of a kind and a pair, total of all dice~%4 of a Kind: total of those 4 dice~%Chance: any combination, total of all dice~%Yacht: 5 of a kind, 50 points~%"
		    :top-bonus nil
		    :y-bonus nil
		    :rules `(:little-straight ("Little Straight" ,#'is-low-straight ,(defscore 30))
			    :big-straight ("Big Straight" ,#'is-high-straight ,(defscore 30))
			    :full-house ("Full House" ,#'is-full-house ,(defscore (total-dice *dice*)))
			    :four-of-a-kind ("4 of a Kind" ,#'is-four-of-a-kind ,(defscore (* 4 arg)))
			    :chance ("Chance" ,#'identity ,(defscore (total-dice *dice*)))
			    :yacht ("Yacht" ,#'is-yahtzee ,(defscore 50))))
	
	:generala
	(make-ygame :name "Generala"
		    :short-description "The Central and South American cousin of Yahtzee."
		    :long-description "A Yahtzee variation popular in Central and South America, older than Yahtzee itself.~%~%Top Section: (same as Yahtzee)~%Top Bonus: Generala has no top bonus~%Bottom Section:~%Straight: a run of 5 but with a unique twist: a 1 can be placed after a six, allowing \"wrap around\" straights, such as 4,5,6,1,2~%Full House: three of a kind plus a pair, 30 points~%4 of a Kind: 40 points~%Generala: 50 points~%First Roll Bonuses: Straight, Full House and 4 of a Kind earn an extra 5 (optional 10) points if made on the first roll of a turn.  Rolling a Generala on the first roll is an automatic win.~%Options:~%-Generala may be worth 60 points~%-Double Generala, an optional score field for rolling a second Generala (worth double the points for a Generala)~%-First Roll Bonus may optionally be 10 points~%"
		    :top-bonus nil
		    :y-bonus `("Double Generala" :generala ,(lambda (ysheet)
							  (incf (ysheet-y-bonus ysheet) (* 2 (game-option :generala-value)))))
		    :options '(:generala-value ("Score for a Generala" 50 60) 
			       :first-roll-bonus ("Bonus for rolling Straith, Full House and 4 of a Kind on the first roll" 5 10) 
			       :wrap-around-straights ("Allow Straights to wrap around (as in 3,4,5,6,1)" t nil) 
			       :y-bonus-p ("Score Bonus points for a second Generala" nil t) 
			       :wild-ones-straight ("Allow ones to be wild in Straights" nil t))
		    :rules `(:straight ("Straight" ,#'is-straight ,(defscore (+ 20 (if (= 1 *roll-num*) 5 0))))
			    :full-house ("Full House" ,#'is-full-house ,(defscore (+ 30 (if (= 1 *roll-num*) 5 0))))
			    :four-of-a-kind ("4 of a Kind" ,#'is-four-of-a-kind ,(defscore (+ 40 (if (= 1 *roll-num*) 5 0))))
			    :generala ("Generala" ,#'is-yahtzee ,(defscore (progn (when (= 1 *roll-num*)
										    (setf *winning-player* *current-player*)) 50)))))
	
	:yatzy
	(make-ygame :name "Yatzy"
		    :short-description "Scandanavia's fun variation"
		    :long-description "This is a public domain Yahtzee variation that is most popular in Scandanavia.  There are more scoring slots and the scoring is different from Yahtzee.~%~%Top Section: same as Yahtzee~%Top Bonus: 50 points~%Bottom Section:~%One Pair: sum of those two dice~%Two Pair: sum of the dice in those two pairs~%Three of a Kind: sum of those three dice~%Four of a Kind: sum of those four dice~%Small Straight: (1,2,3,4,5), 15 points~%Large Straight: (2,3,4,5,6), 20 points~%House: three of a kind and a pair, sum of all the dice~%Yatzy: 5 of a kind, 50 points~%Chance: any combination, sum of all dice~%"
		    :top-bonus (lambda (x) (if (>= x 63) 50 0))
		    :y-bonus nil
		    :options '(:top-bonus-amount ("Top Bonus Amount" 50 35 25)
			       :distinct-house-pairs ("Require Full House and Two Pairs to be made of two distinct numbers" nil t))
		    :rules `(:one-pair ("One Pair" ,#'one-pair ,(defscore (* 2 arg)))
     :two-pairs ("Two Pairs" ,#'two-pair ,(defscore (* 2 (reduce #'+ arg))))
     :three-of-a-kind ("3 of a Kind" ,#'is-three-of-a-kind ,(defscore (* 3 arg)))
     :four-of-a-kind ("4 of a Kind" ,#'is-four-of-a-kind ,(defscore (* 4 arg)))
     :small-straight ("Small Straight" ,#'is-low-straight ,(defscore 15))
     :large-straight ("Large Straight" ,#'is-high-straight ,(defscore 20))
     :house ("House" ,#'is-full-house ,(defscore (total-dice *dice*)))
     :yatzy ("Yatzy" ,#'is-yahtzee ,(defscore 50))
     :chance ("Chance" ,#'identity ,(defscore (total-dice *dice*)))))

	:kismet
	(make-ygame :name "Kismet"
		    :short-description "Adds color to the dice and new fields such as \"flush\""
		    :long-description "This is another commercial Yahtzee variant introduced in 1964.  It adds colors to the dice: black for 1 and 6, red for 2 and 5 and green for 3 and 4.  These colors allow the opportunity for new scoring fields.  Kismet is also designed such that anyone who rolls a second Kismet is very likely to win.~%~%Top Section: same as Yahtzee~%Top Bonus: there are three bonus levels: 35 bonus for scores above 63, 55 points for scores above 70 and 75 points for a score of 78 or more~%Bottom Section:~%Two Pair Same Color: two pairs that are of the same color (4 of a kind, full house and kismet also qualify), sum of all dice~%3 of a Kind: sum of all dice~%Straight: run of 5, 30 points~%Flush: dice of all the same color, 35 points~%Full House: 3 of a kind plus a pair, 15 plus sum of all dice~%Full House Same Color: 20 points plus sum of dice~%4 of a Kind: 25 points plus sum of all dice~%Yarborough: same as \"Chance\" field, sum of all dice~%Kismet: 5 of a kind, 50 points plus sum of all dice~%Kismet Bonus: when any player rolls a second Kismet, all other players must take a zero in the first available score slot from the top and will lose their turn.  This roll may be score in any slot in the bottom section except for Straight.  This applies to any subsequent Kismets as well.~%"
		    :top-bonus (lambda (top-score)
				 (cond ((>= top-score 78) 75)
				       ((>= top-score 71) 55)
				       ((>= top-score 63) 35)
				       (t 0)))
		    :y-bonus nil
		    :options nil
		    :rules `(:two-pair-same-color ("Two Pair Same Color" ,#'two-pair-same-color ,(defscore (total-dice *dice*)))
						  :three-of-a-kind ("3 of a Kind" ,#'is-three-of-a-kind ,(defscore (total-dice *dice*)))     
						  :straight ("Straight" ,#'is-straight ,(defscore 30))
						  :flush ("Flush" ,#'is-flush ,(defscore 35))
						  :full-house ("Full House" ,#'is-full-house ,(defscore (+ 15 (total-dice *dice*))))
						  :full-house-same-color ("Full House Same Color" ,#'is-full-house-same-color
												  ,(defscore (+ 20 (total-dice *dice*))))
						  :four-of-a-kind ("Four of a Kind" ,#'is-four-of-a-kind 
										    ,(defscore (+ 25 (total-dice *dice*))))
						  :yarborough ("Yarborough" ,#'identity ,(defscore (total-dice *dice*)))
						  :kismet ("Kismet" ,#'is-yahtzee ,(defscore (+ 50 (total-dice *dice*))))))
	
	:old-cheerio
	(make-ygame :name "Traditional Cheerio"
		    :short-description "An old English dice game with Wild Ones"
		    :long-description "Traditional Cheerio has a much smaller score card with only 7 fields: the usual Top Section and the Cheerio, which can be either a 5 of a kind or a run of 5.  The twist is that Ones are Wild!  In any score slot, ones can be substituted as long as another die of that number is in the roll.  The downfall of using Wild Ones is that your score will be less.~%~%Top Section: score as usual, but remember that Ones are Wild.~%5 of a Kind Bonus for Top Section: If you roll 5 of a kind for the top section without using any Wild Ones, your score is doubled.  For example, 5 fours will score 40 points instead of 20.~%Cheerio: 5 of a kind or a run of 5.  50 points, 25 if a Wild One is used.~%"
		    :top-bonus nil
		    :y-bonus nil
		    :options nil
		    :rules `(:cheerio ("Cheerio" ,#'is-cheerio ,(defscore (cond ((= arg 1) 25) ((> arg 1) 50) (t 0))))))
	:new-cheerio
	(make-ygame :name "Extended Cheerio"
		    :short-description "A newer take on the old English Cheerio game, but this time without the Wild Ones."
		    :long-description "Extended Cheerio closely resembles Yahtzee except for scoring and names.~%Top Section: same as Yahtzee~%Top Bonus: Extended Cheerio has no Top Bonus~%Bottom Section:~%3 of a Kind: 30 points~%4 of a Kind: 40 points~%5 of a Kind: 50 points~%Run of Four: 30 points~%Cheerio: a run of 5, 40 points~%Full House: three of a kind plus a pair, 40 points~%Sum: the \"Chance\" slot"
		    :top-bonus nil
		    :y-bonus nil
		    :options nil
		    :rules `(:three-of-a-kind ("3 of a Kind" ,#'is-three-of-a-kind ,(defscore 30))
			     :four-of-a-kind ("4 of a Kind" ,#'is-four-of-a-kind ,(defscore 40))
			     :five-of-a-kind ("5 of a Kind" ,#'is-yahtzee ,(defscore 50))
			     :run-of-four ("Run of Four" ,#'is-small-straight ,(defscore 30))
			     :full-house ("Full House" ,#'is-full-house ,(defscore 40))
			     :cheerio ("Cheerio" ,#'is-large-straight ,(defscore 50))
			     :sum ("Sum" ,#'identity ,(defscore (total-dice *dice*)))))
	
	:coal-heat
	(make-ygame :name "Coal Heat"
		    :short-description "A variation by the developers with desperation moves and lifelines"
		    :long-description "Named in honor of my parents who would stay up until the wee hours of the morning in the dead of winter playing Yahtzee while my father kept the coal furnace going to keep us kids warm.  Remembering my parents frustration, I added \"lifelines\" to allow one more chance to score points and a few more scoring fields that we always wished were there.~%Top Section: same as Yahtzee~%Top Bonus: 35 points for a score of 63 or above, 5 points for a score of exactly 62~%Bottom Section:~%One Pair: 5 points plus sum of the two dice~%Two Pair: sum of the four dice in the pairs~%3 of a Kind: sum of all dice~%4 of a kind: sum of all dice~%Full House: 25 points~%Small Straight: run of 4, 30 points~%Large Straight: run of 5, 40 points~%Clinker: 5 of a kind, 50 points~%Chance: any combination, sum of all dice~%Lifelines:~%Desperation Roll: once per game, you may get a 4th roll, but it must be with all 5 dice~%Change a Die: once per game, you may change one die to the value of your choice, but you will sacrifice half the scoring value~%"
		    :top-bonus (lambda (top-score)
				 (cond ((>= top-score 63) 35)
				       ((= top-score 62) 5)
				       (t 0)))
		    :y-bonus `("Clinker Bonus" :clinker ,(lambda (ysheet)
							(incf (getf (ysheet-score-fields ysheet) :y-bonus) 100)))
		    :options '(:y-bonus-p ("Use Clinker Bonus" nil t))
		    :rules `(:one-pair ("One Pair" ,#'one-pair ,(defscore (+ 5 (* 2 arg))))
			    :two-pairs ("Two Pairs" ,#'two-pair ,(defscore (* 2 arg)))
			    :three-of-a-kind ("3 of a Kind" ,#'is-three-of-a-kind ,(defscore (total-dice *dice*)))
			    :four-of-a-kind ("4 of a Kind" ,#'is-four-of-a-kind ,(defscore (+ 10 (total-dice *dice*))))
			    :full-house ("Full House" ,#'is-full-house ,(defscore 25))
			    :small-straight ("Small Straight" ,#'is-small-straight ,(defscore 30))
			    :large-straight ("Large Straight" ,#'is-large-straight ,(defscore 40))
			    :clinker ("Clinker" ,#'is-yahtzee ,(defscore 50))
			    :chance ("Chance" ,#'identity ,(defscore (total-dice *dice*)))))
	
	))

;;; gets the name of a score field.
;;; accepts a symbol, such as :full-house
;;; returns a string corresponding to the name of the bottom section rule
;;; -OR- the symbol is converted to a string in the case of top section rules
(defun score-field-name (game field-name)
  (car (getf (ygame-rules game) field-name)))

;;; the random function that returns a value from 1 to x
(defun drandom (x)
  (+ 1 (random x)))

;;; returns a list of random integers (dice) of num-dice length
(defun roll (num-dice)
  (if (= 0 num-dice)
      nil
      (cons (drandom 6) (roll (1- num-dice)))))

;;; accepts a list of dice and the indices of those to be held
;;; and NOT rolled.  Returns the values corresponding to those
;;; indices in a list.
(defun hold (dice &rest indices)
  (loop for i in indices collecting (nth (1- i) dice)))

;;; returns the held dice (according to indices) plus the newly rolled/random
;;; dice values
(defun hold-and-roll (dice &rest indices)
  (append (apply 'hold (cons dice indices)) (roll (- 5 (length indices)))))

;;; returns zero if nil, otherwise x
(defun nil-zero (x)
  (if (null x)
      0
      x))

;;; returrns the top section of a player's score sheet
(defun top-section (ysheet)
  (subseq (ysheet-score-fields ysheet) 0 12))

;;; returns the bottom section of a player's score sheet
(defun bottom-section (ysheet)
  (let ((sheet (ysheet-score-fields ysheet)))
    (subseq sheet 12 (length sheet))))
    
;;; sums the scores in a plist type score sheet
(defun sum-scores (plist)
  (if (null plist)
      0
      (+ (nil-zero (cadr plist))
	 (sum-scores (cddr plist)))))

;;; returns the sum of the top section scores
(defun top-section-score (ysheet)
  (sum-scores (top-section ysheet)))

;;; returns the sum of the bottom section scores
(defun bottom-section-score (ysheet)
  (sum-scores (bottom-section ysheet)))

;;; returns the sum of the top section scores, top bonus,
;;; bottom section and yahtzee/5 of a kind bonus
(defun total-score (ysheet)
  (+ (top-section-score ysheet) (nil-zero (ysheet-top-bonus ysheet))
     (bottom-section-score ysheet) (nil-zero (ysheet-y-bonus ysheet))))

;;; uses the current value of global variable *dice*
;;; to check for and score a given scoring slot for a given game
;;; Eg. (score-slot :full-house :generala) checks *dice* to see if they
;;; fulfill Generala's Full House requirements then calculates the score
;;; according to Generala's scoring rule
(defun score-slot (slot game)
  (let* ((slot-info (getf (ygame-rules game) slot))
	 (score-check-result (funcall (second slot-info) *dice*)))
    (if score-check-result
	(funcall (third slot-info) score-check-result)
	0)))

;;; traverses the rules plist of a game and produces a score-field
;;; list for use in the ysheet struct.  For example, this would accept
;;; Generala's rules and return a list as such:
;;; (:straight nil :full-house nil :four-of-a-kind nil :generala nil)
(defun get-score-fields (rules)
  (if (null rules)
      nil
      (cons (car rules) (cons nil (get-score-fields (cddr rules))))))

;;; structure defining a player's score sheet.
(defstruct ysheet
  (player-name)  ; string containing the name of the player
  (score-fields) ; property list of score fields
  (top-bonus)    ; top bonus score (or NIL if not applicable)
  (y-bonus))     ; yahtzee/5-of-a-kind bonus (or NIL if not applicable)

;;; creates a ysheet (player score sheet)
;;; requires the game being played and a player name
;;; score fields, top-bonus and y-bonus are determined by the ygame struct
;;; for the game passed to the function
(defun make-score-sheet (game player-name)
  (make-ysheet :player-name player-name
	       :score-fields (get-score-fields (ygame-rules game))
	       :top-bonus (if (null (ygame-top-bonus game)) nil 0)
	       :y-bonus (if (ygame-y-bonus game) 0 nil)))

;;; print's a player's full score sheet.  NOT FINISHED
(defun print-ysheet (ysheet)
  (progn
    (format t "~A Score Sheet~%Player Name: ~A~%" (ygame-name *selected-game*)
	    (ysheet-player-name ysheet))))

;;; creates a list of ysheets based on the selected game and a list of 
;;; player names
(defun make-players (game &rest players)
  (mapcar (lambda (p)
	    (make-score-sheet game p))
	  players))

;;; applies a yahtzee bonus (or whatever it's called)
;;; gets the info from the selected game
;;; checks to see if there's already a score for the yahtzee type field
;;; then calls the function from the game rules to apply the bonus
(defun apply-y-bonus (ysheet)
  (let* ((y-bonus-info (ygame-y-bonus *selected-game*))
	(score-field (second y-bonus-info))
	(y-bonus-func (third y-bonus-info)))
    (when (getf (ysheet-score-fields ysheet) score-field)
      (funcall y-bonus-func ysheet))))

;;; commits a player's score to their ysheet score-fields
;;; you must pass the slot name, the player ysheet.  it will use the
;;; global variable *selected-game* to determine the game
;;; chesks for the presence of a y-bonus field and a yahtzee roll
;;; this is independent of the score field chosen by the player
(defun commit-score (slot player)
  (let ((player-slots (ysheet-score-fields player)))
    (progn
      (when (and (ygame-y-bonus *selected-game*) (is-yahtzee *dice*))
	  (apply-y-bonus player))
      (let ((score (setf (getf player-slots slot)
			 (score-slot slot *selected-game*)))
	    (top-bonus-func (ygame-top-bonus *selected-game*)))
	(format t "~A scored ~A in the ~A category!~%"
		(ysheet-player-name player)
		score
		(score-field-name *selected-game* slot))
	(when top-bonus-func
	  (setf (ysheet-top-bonus player) 
		(funcall (ygame-top-bonus *selected-game*) 
			 (top-section-score player))))))))

;;; a prompt/loop combination to ask for user's input
;;; this should probably be a more generic function that takes parameters
;;; such as the prompt(s), acceptable values and error message
(defun roll-or-score (roll)
  (progn 
    (format t "Dice (Roll ~A): ~A~%" roll *dice*)
    (format t "[s]core results ~A -> " (if (= *max-rolls* roll)
					   "" "[r]oll again"))
    (let ((choice (read)))
      (if (not (find choice '(r s)))
	  (roll-or-score (1+ roll))
	  choice))))

(defun print-score-sheet (player)
  (let* ((score-card (ysheet-score-fields player))
	 (width (longest-field-name score-card))
	 (format-string (format nil "~A~A~A" "~4@A ~" width ",,,'.A: ~A~%")))
    (labels ((print-score-fields (n fields)
	       (when fields
		 (format t format-string (bnum n) (score-field-name *selected-game* (car fields)) (cadr fields))
		 (print-score-fields (1+ n) (cddr fields)))))
      (progn
	(print-score-fields 1 (top-section player))
	(format t format-string " " "Top Score" (top-section-score player))
	(if (ysheet-top-bonus player)
	    (format t format-string " " "Top Bonus" (ysheet-top-bonus player)))
	(print-score-fields 7 (bottom-section player))
	(if (ysheet-y-bonus player)
	    (format t format-string " " (first (ygame-y-bonus *selected-game*))
		    (ysheet-y-bonus player)))
	(format t format-string " " "Bottom Score" (bottom-section-score player))
	(format t format-string " " "Total Score" (total-score player))))))

;;; prompts the player requesting a score field to be chosen
;;; from the list to which a score will be applied
(defun prompt-score (player)
  "Return the name of the score field chosen from a list"
  (let ((score-card (ysheet-score-fields player)))
    (labels ((get-input ()
	       (format t "Enter number of score field: ")
	       (let ((choice (read)))
		 (if (or (not (integerp choice)) (< choice 0) (> choice 
								 (/ (length score-card) 2)))
		     (progn
		       (format t "Invalid number. ")
		       (get-input))
		     (if (getf score-card (nth-slot choice))
			 (progn
			   (format t "There is already a score there. ")
			   (get-input))
			 choice))))
	     (nth-slot (n)
	       (nth (- (* 2 n) 2) score-card)))
      (progn
	(print-score-sheet player)
	(format t "~%Dice Values:~{   ~A~}~%~%" *dice*)
	(nth (- (* 2 (get-input)) 2) score-card)))))

;;; dumb little helper function that prints a number in brackets
;;; so I can use this little string with format field widths
;;; there's probably a format spell that can do it but I don't know it yet
(defun bnum (n)
  (format nil "[~A]" n))

;;; finds the logest field name in a plist of score fields
(defun longest-field-name (score-fields)
  (if (null score-fields)
      0
      (max (length (score-field-name *selected-game* (car score-fields)))
	   (longest-field-name (cddr score-fields)))))

;;; prompts the user requesting a list of dice to hold for a roll
;;; NEEDS INPUT VALIDATION
(defun prompt-hold ()
  "Return a list of dice indices to hold for a next roll"
  (progn
    (format t "Die#    [1] [2] [3] [4] [5]~%")
    (format t "Values~{   ~A~}~%" *dice*)
    (format t "Enter the die numbers of those you wish to HOLD: ")
    (read-from-string (format nil "(~A)" (read-line)))))

;;; roll dice (1), choose to score or hold-and-roll (2),
;;; choose to score or hold-and-roll (3), must score
(defun take-turn (player)
  (labels ((roll-sequence (rollnum)
	     (setf *roll-num* rollnum)
	     (if (= *max-rolls* rollnum)
		 (commit-score (prompt-score player) player)
		 (case (roll-or-score rollnum)
		   (r (progn
			(setf *dice* (apply #'hold-and-roll *dice* (prompt-hold)))
			(roll-sequence (1+ rollnum))))
		   (s (commit-score (prompt-score player) player))))))
    (format t "~%~A's turn!~%" (ysheet-player-name player))
    (print-score-sheet player)
    (setf *dice* (roll 5))
    (roll-sequence 1)))
      
(setf *roll-num* 1)

;;; converts a string to a keyword symbol.  replaces spaces with dashes
(defun name-to-symbol (string)
  (intern (substitute #\- #\space (string-upcase string)) "KEYWORD"))

;;; converts a keyword symbol to a string. replaces dashes with spaces
(defun symbol-to-name (symbol)
  (string-capitalize (substitute #\space #\- (symbol-name symbol))))

(defun prompt-num-players ()
  (format t "Enter # of players: ")
  (let ((num (read)))
    (if (and (integerp num) (> num 0))
	num
	(progn
	  (format t "Invalid number.")
	  (prompt-num-players)))))

;;; a loop that gets a given number of player names and returns the list
(defun get-players (num)
  (if (= 0 num)
      nil
      (progn
	(format t "Enter player name: ")
	(cons (read-line) (get-players (1- num))))))

;;; For use in the game selection menu
;;; prints the information for one single game.  requires a number
;;; and a ygame struct
(defun print-game-short (n ygame)
  (format t "   [~A] ~A - ~A~%" n (ygame-name ygame) (ygame-short-description ygame)))

;;; Prints all games for the game selection menu
(defun print-games ()
  (progn
    (format t "Welcome to Yahtzee World! Please choose from among the following variations of the classic dice rolling game of luck and strategy!~%~%")
    (labels ((print-games (n games)
	       (if (null games)
		   nil
		   (progn
		     (print-game-short n (cadr games))
		     (print-games (1+ n) (cddr games))))))
      (print-games 1 *games*))
    (format t "   [0] View a more detailed description and rules for one of the above variations.~%")))

;;; gets the nth game symbol in the global *games* variable
(defun get-nth-game (n)
  (nth (- (* 2 n) 2) *games*))

;;; gets the nth ygame struct in the global *games* variable
;;; seems kinda lame to have both of these functions
(defun get-nth-ygame (n)
  (nth (1- (* 2 n)) *games*))

;;; prompts user requesting the choice of Yahtzee variation to be played
;;; validates input
;;; allows user to view long description of a game, including its rules
;;; returns the plist symbol for a ygame
(defun get-game ()
  (progn
    (format t "~%################################################################~%")
    (print-games)
    (labels ((get-opt (prompt)
	       (format t prompt)
	       (let ((num (read)))
		 (if (or (not (integerp num)) (< num 0) (>  num 8))
		     (progn
		       (format t "Not a valid option. ")
		       (get-opt prompt))
		     num))))
      (let ((option (get-opt "Enter a number: ")))
	(if (= 0 option)
	    (progn
	      (format t "----------------------------~%")
	      (format t (ygame-long-description (get-nth-ygame (get-opt "# of variation: "))))
	      (format t "----------------------------~%")
	      (format t "[PRESS ENTER TO CONTINUE]")
	      (read-line)
	      (get-game))
	    (get-nth-game option))))))

;;; takes an yahtzee variation option and returns the pretty value
;;; basically this is so T and NIL options will be printed as "Yes/No"
;;; there may be a format recipe for that but I don't know it yet.
(defun pprint-option (opt)
  (cond ((null opt) "No")
	((equal opt T) "Yes")
	(t opt)))

;;; accepts the description of an option, such as "Allow Wrap Around Straights"
;;; and a list of the options, such as "Yes" or "No" or 50 or 60
;;; prints the options, validates input and returns the actual option itself
;;; rather than the printed option, so T instead of "Yes"
(defun choose-option (desc options)
  (progn
    (format t "~A~%" desc)
    (dotimes (x (length options))
      (format t "   [~A] ~A~%" (1+ x) (pprint-option (nth x options))))
    (labels ((f ()
	       (format t "Enter a number; ")
	       (let ((num (read)))
		 (if (or (not (integerp num)) (<= num 0) (> num (length options)))
		     (f)
		     (nth (1- num) options)))))
      (f))))

;;; prints the available options for a given Yahtzee variation
;;; and prompts to user to set them
;;; perhaps this should be something saveable.  Save default options in
;;; a config file
(defun set-options (options-list)
  (if (null options-list)
      nil
      (let ((sym (car options-list))
	    (desc (caadr options-list))
	    (options (cdadr options-list)))
	(cons sym 
	      (cons (choose-option desc options)
		    (set-options (cddr options-list)))))))

;;; sets the *selected-game* global variable according to the ygame
;;; selected.  Also prompts the user to set the desired options for that
;;; variation
(defun set-selected-game (ygame-name)
  (let ((ygame (getf *games* ygame-name)))
    (progn
      (setf *selected-game* (copy-ygame ygame))					
      (setf (ygame-rules *selected-game*)
	  (append (make-top-section *top-slots* ygame-name)
		  (ygame-rules *selected-game*)))
      (let ((goptions (ygame-options *selected-game*)))
	(when goptions
	  (setf (ygame-options *selected-game*)
		(set-options goptions))
	  (unless (getf (ygame-options *selected-game*) :y-bonus-p)
	    (setf (ygame-y-bonus *selected-game*) nil))))
      *selected-game*)))
  
;;; gets the winning player ysheet
;;; if *winning-player* was set by a Big Generala (1st roll Generala)
;;; or any other potential game terminating event, it will simply return it
;;; otherwise it simply gets the max score of all players
(defun get-winner ()
  (or *winning-player* 
      (car (sort *players* (lambda (p1 p2)
			     (> (total-score p1) (total-score p2)))))))
;      (apply #'max (mapcar #'total-score *players*))))

;;; uses the results of get-game to find the particular game
;;; set the *selected-game* variable and start the main game loop
;;; prompts the user for the number of players
(defun start-game (ygame)
  (let ((rounds (ygame-rounds ygame)))
    (progn
      (setf *players* (apply #'make-players ygame (get-players (prompt-num-players))))
      (do ((r 1 (1+ r)))
	  ((or *winning-player* (= r (1+ rounds))) (get-winner))
	(dolist (player *players*)
	  (take-turn player))))))

;;; the main game function.  It's a format that accepts two arguments:
;;; the ysheet player name retrieved from the results of the game loop
;;; and the name of the selected game
(defun yahtzee ()
  (let ((winning-player (start-game (set-selected-game (get-game)))))
    (format t "Congratulations ~A! You have won this game of ~A with a score of ~A~%"
	  (ysheet-player-name winning-player)
	  (ygame-name *selected-game*)
	  (total-score winning-player))))
