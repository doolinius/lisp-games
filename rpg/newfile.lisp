
;;;; TODO - level-up systems, money
;;;;        character class spells and abilities
;;;;        special items, army & party membership
;;;;        spell/magic acquisition, circle of magic
;;;;        MP system (exhaustion states)
;;;;          (overcasting requirement... cast with 0 MP)
;;;;        accessories (battle actions)
;;;;        special skills?
;;;;        something fun/unique for weapons upgrade, maybe tied with special
;;;;            attacks
;;;;
;;;;        weapon types - sword, axe, hammer, bow, crossbow, spear, lance,
;;;;                       staff, rod, knife, katana, gloves, throwing
;;;;        weapon enchantments (new field?)
;;;;        level requirement for weapons?
;;;;        item drops (another field in "extras" p-list)
;;;;
;;;; calculations - experience (if using 100 point system)
;;;;              - level-up stat gains (character classes?  individualized?)
;;;;

(defvar *critical-damage* 1.75)
(defvar *second-attack-max* 25)

;;; for the random number generator
(setf *random-state* (make-random-state t))

;;; returns 0 if x is null, otherwise returns x
(defun nil-zero (x)
  (if (null x) 0 x))

(defun percentile (stat)
  (< (1+ (random 100)) stat))

(defun div (x y)
  (coerce (/ x y) 'float))

(defun rround (num)
  (round (+ 0.01 num)))

;;;
;;; SPELLS
;;;

;;; class definition for spells. only subclasses will be used
;;;  sub-types: attack, healing, status, other (Egress, Illusion, etc)
(defclass spell ()
  ((name :initarg :name :accessor name)
   (mp-use :initarg :mp-use :accessor mp-use)
   (range :initarg :range :accessor range)
   (area :initarg :area :accessor area)
   (next-level :initarg :next-level :accessor next-level)))

;;; sub-class for attack spells adds damage and elemental fields
(defclass atk-spell (spell)
  ((damage :initarg :damage :reader damage)
   (elemental :initarg :elemental :reader elemental)))

;;; sub-class for healing spells adds hp-restore slot
(defclass heal-spell (spell)
  ((hp-restore :initarg :hp-restore :reader hp-restore)))

;;; sub-class for spells that effect statuses
(defclass status-spell (spell)
  ((statuses :initarg :statuses :reader statuses)))

(defclass stat-boost-spell (spell)
  ((stat-boosts :initarg :stat-boosts :reader stat-boosts)
   (status-name :initarg :status-name :reader status-name)))

;;; helper functions for creating spells
(defun make-atk-spell (name mp-use range area next-level damage elemental)
  (make-instance 'atk-spell :name name :elemental elemental :damage damage :mp-use mp-use :range range :area area :next-level next-level))

(defun make-heal-spell (name mp-use range area next-level hp-restore)
  (make-instance 'heal-spell :name name :mp-use mp-use :range range :area area :next-level next-level :hp-restore hp-restore))

(defun make-status-spell (name mp-use range area next-level statuses)
  (make-instance 'status-spell :name name :mp-use mp-use :range range :area area :next-level next-level :statuses statuses))

(defun make-stat-boost-spell (name mp-use range area next-level stat-boosts status-name)
  (make-instance 'stat-boost-spell :name name :mp-use mp-use :range range :area area :next-level next-level :stat-boosts stat-boosts :status-name status-name))

;;; generic functions for spell actions
(defgeneric spell-action (spell caster target))

(defmethod spell-action ((spell atk-spell) caster target)
  (magic-atk spell caster target))

(defmethod spell-action ((spell heal-spell) caster target)
  (magic-heal spell caster target))

(defmethod spell-action ((spell status-spell) caster target)
  (apply #'set-statuses target (statuses spell)))

(defmethod spell-action ((spell stat-boost-spell) caster target)
  (progn
    (apply #'set-boosts target (stat-boosts spell))
    (set-status target (status-name spell) 3)))

;;; This may need an accompanying data structure to organize the spells
;;; according to elemental, type, level, etc.
(defparameter *spells*
  (list :fire (make-atk-spell "Fire" 3 2 1 :fire2 7 :fire)
	:fire2 (make-atk-spell "Fire 2" 6 2 2 :fire3 12 :fire)
	:ice (make-atk-spell "Ice" 4 2 2 :ice2 10 :ice)
	:lightning (make-atk-spell "Lightning" 7 3 2 :lightning2 12 :lightning)
	:heal (make-heal-spell "Heal" 3 1 1 :heal2 10)
	:heal2 (make-heal-spell "Heal 2" 5 2 1 :heal3 12)
	:boost (make-stat-boost-spell "Boost" 8 2 1 :boost2 '(strength 5 agility 5) :boost)
	:detox (make-status-spell "Detox" 2 2 1 :detox2 '(:poisoned nil))))

;;;
;;; CHARACTER CLASSES
;;;

;;; character class definition
;;; TODO: special attacks and spells, healing item bonus
;;; NOT IMPLEMENTED: terrain-effects and class-res
(defclass char-class ()
  ((name :initarg :name :accessor name)
   (weapons :initarg :weapons :accessor weapons)
   (stat-bonuses :initarg :stat-bonuses :initform nil :accessor stat-bonuses)
   (heal-bonus :initarg :heal-bonus :initform 1 :accessor heal-bonus)
   (promotion-classes :initarg :promotion-classes :accessor promotion-classses)
   (terrain-effects :initarg :terrain-effects :accessor terrain-effects) 
   (class-res :initarg :class-res :initform nil :accessor class-res)
   ))

;;; helper function for creating instances of char-class
(defun make-char-class (name weapons stat-bonuses promotion-classes &key (heal-bonus 1))
  (make-instance 'char-class :name name :weapons weapons :stat-bonuses stat-bonuses :promotion-classes promotion-classes :heal-bonus heal-bonus))

;;; method to handle nil character classes for enemies
(defmethod class-res ((cclass (eql nil)))
  nil)

;;; macro for creating generic method for accessing stat bonuses of char-class
(defmacro make-cclass-accessor (stat)
  (let ((method-name (intern (concatenate 'string (symbol-name stat) (symbol-name '#:-stat)))))
    `(progn
       (defgeneric ,method-name (char-class))
       (defmethod ,method-name ((cc char-class))
	 (nil-zero (getf (stat-bonuses cc) (quote ,stat)))))))

;;; macro that loops through various stats to create accessors
(defmacro make-cclass-accessors (&rest stats)
  `(progn
     ,@(loop for stat in stats
	    collecting `(make-cclass-accessor ,stat))))

;;; call to create the accessor functions for the stats
(make-cclass-accessors strength defense intelligence agility luck move critical)

;;; global list of character classes
(defparameter *char-classes*
  (list
   ;; base classes
   :fighter   (make-char-class "Fighter" '(sword) nil '(:swordsman :samurai))
   :warrior   (make-char-class "Warrior" '(axe) nil '(:gladiator :warlord))
   :archer    (make-char-class "Archer" '(bow) nil '(:sniper :ranger))
   :mage      (make-char-class "Mage" '(staff rod) nil '(:fire-mage :healer :mind-mage :light-mage :chronomancer :storm-wizard))
   :monk      (make-char-class "Monk" '() nil '(:master-monk :mystic) :heal-bonus 1.25)
   :knight    (make-char-class "Knight" '(spear lance) nil '(:paladin :cavalier))
   :thief     (make-char-class "Thief" '(knife) nil '(:ninja :assassin))
   :bard      (make-char-class "Bard" '(knife throwing) nil '(:minstrel :troubador))
   ;; promotion classes for base classes
   :swordsman (make-char-class "Swordsman" '(sword two-handed-sword) '(strength 4)  nil)
   :samurai   (make-char-class "Samurai" '(sword katana) '(strength 2 defense 3) nil)
   :gladiator (make-char-class "Gladiator" '(axe sword spear) '(defense 4) nil)
   :warlord   (make-char-class "Warlord" '(axe hammer) '(strength 5) nil)
   :sniper    (make-char-class "Sniper" '(bow crossbow) '(agility 15) nil)
   :ranger    (make-char-class "Ranger" '(bow knife) '(move 1) nil :heal-bonus 2)
   :master-monk (make-char-class "Master Monk" '(glove) nil nil :heal-bonus 1.25)
   :mystic    (make-char-class "Mystic" '(sword) nil nil :heal-bonus 1.5)
   :paladin   (make-char-class "Paladin" '(spear lance sword) nil nil :heal-bonus 1.25)
   :cavalier  (make-char-class "Cavalier" '(spear lance) nil nil)
   :ninja     (make-char-class "Ninja" '(knife katana throwing) nil nil)
   :assassin  (make-char-class "Assassin" '(knife crossbow) nil nil)
   :minstrel  (make-char-class "Minstrel" '(knife throwing) nil nil)   
   :troubador (make-char-class "Troubador" '(knife throwing) nil nil); heal bonus?
   ;; mage promotion classes
   :fire-mage (make-char-class "Fire Mage" '(staff rod) nil nil :heal-bonus 1.25)
   :healer (make-char-class "Healer" '(staff rod) nil nil :heal-bonus 1.5)
   :mind-mage (make-char-class "Mind Mage" '(staff rod) nil nil :heal-bonus 1.25)
   :light-mage (make-char-class "Light Mage" '(staff rod) nil nil)
   :chronomancer (make-char-class "Chronomancer" '(staff rod) nil nil)
   :storm-wizard (make-char-class "Storm Wizard" '(staff rod) nil nil)
   ;; special classes.  character specific, testing, etc.
   :dark-knight (make-char-class "Dark Knight" '(sword two-handed-sword axe) nil nil)
   :holy-knight (make-char-class "Holy Knight" '(lance sword two-handed-sword) nil nil)
   :badass (make-char-class "Badass" '(sword axe lance spear staff axe hammer bow) '(strength 10 defense 10 agility 10 intelligence 10 move 2 luck 10 critical 30)  nil :heal-bonus 2.0)
   ))

;;;
;;; ITEMS
;;;

;;; sub-types: healing, weapons, rings, accessories, boost, promotion, battle, special
;;; base class for items
(defclass item ()
  ((name :initarg :name :accessor name)
   (price :initarg :price :accessor price)
   (use-action :initarg :use-action :accessor use-action)
   (consumable :initarg :consumable :accessor consumable)
   (special :initarg :special :accessor special-p :initform nil)))

;;; subclass for healing items
(defclass healing-item (item)
  ((hp-restore :initarg :hp-restore :reader hp-restore)
   (mp-restore :initarg :mp-restore :reader mp-restore))
  (:default-initargs 
    :mp-restore 0
    :hp-restore 0))

;;; subclass for status changing items (mostly for curing them, but who knows?)
(defclass status-item (item)
  ((statuses
    :initarg :statuses
    :reader statuses)))

;;; subclass for special promotion items
;;; required class is the character class the target must currently have
;;; required level is the character level required for the item to be used
;;; new class is the ccharacter class that will be obtained by using the item
(defclass promotion-item (item)
  ((required-class :initarg :required-class :reader required-class)
   (required-level :initarg :required-level :reader required-level)
   (new-class :initarg :new-class :reader new-class))
  (:default-initargs
   :required-level 20))

;;; items for permanent stat boosts
;;; stat-boosts: a list of stat names (symbols) and boost amount (integer)
;;;     (e.g., '(strenth 4))
(defclass boost-item (item)
  ((stat-boosts :initarg :stat-boosts :accessor stat-boosts)))

(defparameter *boost-items*
  (list
   :str-potion (make-instance 'boost-item :name "Strength Potion" :price 500 :consumable t :stat-boosts '(strength 4))))

;;; applies a permanent stat boost to a character
;;; the amount boosted is a random number from 1 to the max allowed by
;;; the item being used
;;; this doesn't appear to be the method used for temporary stat boosts
(defmacro apply-stat-boost (gchar stat max)
  (let ((amount (1+ (random max)))
	(fname (intern (format nil "~A-STAT" stat))))
    `(progn
       (incf (,fname ,gchar) ,amount)
       ,amount)))

(defmacro apply-stat-boosts (gchar &rest stats)
  `(progn
     ,@(loop while stats collecting
	    `(apply-stat-boost ,gchar ,(pop stats) ,(pop stats)))))

(defparameter *healing-items*
  (list
   :herb (make-instance 'healing-item :name "Healing Herb" :price 10 :consumable t :hp-restore 10 :use-action nil)
   :seed (make-instance 'healing-item :name "Healing Seed" :price 30 :consumable t :hp-restore 30 :use-action nil)
   :tear (make-instance 'healing-item :name "Fairy Tear" :price 1000 :consumable t :mp-restore 'full)))

;;; subclass for rings (provide stat bonuses and can be used to cast spells)
;;; TODO: durability, item breakage
(defclass ring (item)
  ((strength-bonus :initarg :str :accessor strength-stat)
   (defense-bonus :initarg :def :accessor defense-stat)
   (agility-bonus :initarg :agi :accessor agility-stat)
   (intelligence-bonus :initarg :int :accessor intelligence-stat)
   (luck-bonus :initarg :luk :accessor luck-stat)
   (move-bonus :initarg :mov :accessor move-stat)
   (critical-bonus :initarg :crit :accessor critical-stat))
  (:default-initargs
    :str 0
    :def 0
    :agi 0
    :int 0
    :luk 0
    :mov 0
    :crit 0))

;;; helper function for creating instances of rings
(defun make-ring (name price &key (use-action nil) (str 0) (def 0) (agi 0) (int 0) (luk 0) (mov 0) (crit 0))
  (make-instance 'ring :name name :price price :use-action use-action :consumable nil :str str :def def :agi agi :int int :luk luk :mov mov :crit crit))

;;; global list of rings
(defparameter *rings*
  (list
   :power-ring (make-ring "Power Ring" 3500 :str 10 :luk -2)))

;;; item subclass for weapons
;;; while primarily used for attack enhancement, some weapons may
;;; also provide bonuses or penalties (such as agility reduction)
;;; others may affect statuses (poison weapon)
(defclass weapon (item)
  ((wpn-type :initarg :wpn-type :accessor wpn-type)
   (strength :initarg :str :accessor strength-stat)
   (defense :initarg :def :accessor defense-stat)
   (intelligence :initarg :int :accessor intelligence-stat)
   (agility :initarg :agi :accessor agility-stat)
   (luck :initarg :luk :accessor luck-stat)
   (move :initarg :mov :accessor move-stat)
   (critical :initarg :crit :accessor critical-stat)
   (range :initarg :range :accessor range)
   (cursed :initarg :cursed :accessor cursed)
   (elemental :initarg :elemental :accessor elemental)
   (status :initarg :status :accessor status))
  (:default-initargs :def 0 :int 0 :agi 0 :luk 0 :crit 0 :mov 0
		     :cursed nil :elemental :physical :status nil))

;;; helper function for making instances of weapons
(defun make-weapon (name price wpn-type str range &key (use-action nil) (def 0) (int 0) (agi 0) (luk 0) (crit 0) (mov 0) (cursed nil) (elemental :physical) (status nil))
  (make-instance 'weapon :name name :price price :use-action use-action :consumable nil :wpn-type wpn-type :str str :def def :int int :agi agi :luk luk :mov mov :crit crit :range range :cursed cursed :elemental elemental :status status))

;;; global list of weapons
(defparameter *weapons*
  (list
   :short-sword (make-weapon "Short Sword" 200 'sword 5 1)
   :heron-blade (make-weapon "Heron Blade" 5000 'sword 25 1)
   :blackstaff (make-weapon "Black Staff" 1500 'staff 18 1)
   :venom-dagger (make-weapon "Venom Dagger" 2000 'knife 10 1 :status :poison)
   :heat-axe (make-weapon "Heat Axe" 2500 'axe 18 1 :use-action :fire2)
   :ruby-dagger (make-weapon "Ruby Dagger" 2800 'knife 15 1 :cursed t :status :poison)))

;;; predicate to check if an item is a weapon
(defun weapon-p (item)
  (equal (type-of item) 'weapon))

;;; predicate to check if an item is a ring
(defun ring-p (item)
  (equal (type-of item) 'ring))

;;; predicate to check if an item is an accessory
(defun accessory-p (item)
  (equal (type-of item) 'accessory))

;;; predicate to determine if an item is equippable
(defun equippable (item)
  (or (ring-p item) (weapon-p item) (accessory-p item)))

(defgeneric item-action (item user target))

;;; use action for healing items
(defmethod item-action ((item healing-item) user target)
  (progn
    (when (/= (hp-restore item) 0)
      (if (equal (hp-restore item) 'full)
	  (apply-healing target (- (max-hp target) (hp target)))
	  (apply-healing target (round (+ 0.01 (* (hp-restore item) (heal-bonus (char-class user))))))))
    (when (/= (mp-restore item) 0)
      (if (equal (mp-restore item) 'full)
	  (incf (mp target) (- (max-mp target) (mp target)))
	  (incf (mp target) (min (mp-restore item) (- (max-mp target) (mp target))))))))

;;; use action for weapons (will always be a spell)
(defmethod item-action ((item weapon) user target)
  (spell-action (getf *spells* (use-action item)) user target))

;;; use action for a status affecting item
(defmethod item-action ((item status-item) user target)
  (set-statuses target (statuses item)))

;;; use action for a stat boosting item
;;; NOT YET IMPLEMENTED
(defmethod item-action ((item boost-item) user target)
  ())

;;;
;;; CHARACTERS
;;;

;;; class definition for characters, both enemies and force members
(defclass gcharacter ()
  ((name :initarg :name :accessor name)
   (char-class :initarg :char-class :accessor char-class)
   (level :initarg :level :accessor level)
   (exp-pts :initarg :exp-pts :initform 0 :accessor exp-pts)
   (max-hp :initarg :max-hp :accessor max-hp)
   (hp :initarg :hp :accessor hp)
   (max-mp :initarg :max-mp :accessor max-mp)
   (mp :initarg :mp :accessor mp)
   (strength :initarg :str :accessor strength-stat)
   (defense :initarg :def :accessor defense-stat)
   (intelligence :initarg :int :accessor intelligence-stat)
   (agility-stat :initarg :agi :accessor agility-stat)
   (luck-stat :initarg :luk :accessor luck-stat)
   (move :initarg :mov :accessor move-stat)
   (critical :initarg :crit :initform '(7 1.5) :accessor critical-stat)
   (boosted-stats :initform '() :accessor boost)
   (weapon :initarg :weapon :initform nil :accessor weapon)
   (ring :initarg :ring :initform nil :accessor ring)
   (accessory :initarg :accessory :initform nil :accessor accessory)
   (items :initarg :items :accessor items :initform (make-list 6))
   (spells :initarg :spells :accessor spells 
	   :initform (make-array 12 :initial-element nil))
   (statuses :initarg :statuses :initform nil :accessor statuses)
   (resistance :initarg :resistance :accessor res :initform '())
   (kills :initarg :kills :accessor kills :initform 0)
   (defeats :initarg :defeats :accessor defeats :initform 0)
   ;; an extra field.  currently using for money from enemies
   ;; also possibly for circle of magic properties
   ;; possibly for non-weapon attack elementals (bats causing sleep, etc)
   (extra :initarg :extra :accessor extra :initarg nil)))

;;; concatenates a symbol and a postfix and returns a function referred to
;;;  by the new string
(defun get-func (stat-name postfix)
  (symbol-function
   (intern 
    (concatenate 'string (symbol-name stat-name) (symbol-name postfix)))))

(defmacro defstat (object stat)
  (let ((fname (intern (format nil "~A-~A" object stat)))
	(mname (intern (format nil "~A-STAT" stat))))
    `(defun ,fname (gchar)
       (if (null (,object gchar))
	   0
	   (,mname (,object gchar))))))

(defun set-boost (target stat value)
  (if (null value)
      (remf (boost target) stat)
      (setf (getf (boost target) stat) value)))

(defun set-boosts (target &rest stats)
  (loop while stats do
       (set-boost target (pop stats) (pop stats))))

(defun set-status (target status value)
  (if (null value)
      (remf (statuses target) status)
      (unless (/= 0 (resistance target status))
	(setf (getf (statuses target) status) value))))

(defun set-statuses (target &rest statuses)
  (loop while statuses do
       (set-status target (pop statuses) (pop statuses))))

;;; generates a stat accessor function for a character
;;;    combines base STR, weapon STR, ring STR and char-class STR bonus
;;;    to calculate the total STR value for attacks
(defmacro stat-accessor (stat)
  (let ((char-fname (intern (format nil "~A-~A" stat 'stat)))
	(weapon-fname (intern (format nil "~A-~A" 'weapon stat)))
	(ring-fname (intern (format nil "~A-~A" 'ring stat)))
	(char-class-fname (intern (format nil "~A-~A" 'char-class stat))))
    `(progn
       (defstat weapon ,stat)
       (defstat ring ,stat)
       (defstat char-class ,stat)
       (defun ,stat (gchar) ; defines the overall stat function for a gchar
	 (+ ; add the following:
	  (,char-fname gchar) ; character stat
	  (,char-class-fname gchar) ; character class stat bonus
	  (,ring-fname gchar) ; ring stat bonus
	  (nil-zero (getf (boost gchar) (quote ,stat))) ; temporary boost bonus
	  (,weapon-fname gchar)))))) ; weapon stat
  
;;; takes a list of stats for which to make accessor functions
(defmacro make-stat-fns (&rest stats)
  `(progn
     ,@(loop for stat in stats
	  collecting `(stat-accessor ,stat))))

;;; expression to create the accessors for the following stats
;;; if a new stat is added, it merely needs to be added to this expression
(make-stat-fns strength defense intelligence agility luck critical move)

;;; get the atk-elemental field from the extra slot
;;; this allows weaponless enemies to have an attack elemental
(defun extra-elemental (gchar)
  (getf (extra gchar) :atk-elemental))

;;; get the overall attack elemental of the character
;;;     weapon takes precedence, then extra-atk-elemental, then just :physical
(defun atk-elemental (gchar)
  (let ((w (weapon gchar))
	(ae (extra-elemental gchar)))
    (cond
      (w (elemental (weapon gchar)))
      (ae ae)
      (t :physical))))

(defgeneric resistance (gcharacter type))

;;; method that calculates elemental and status resistances
;;; based on those of the character and the character class
(defmethod resistance ((gchar gcharacter) type)
  (let ((char-resist (nil-zero (getf (res gchar) type)))
	(class-resist (nil-zero (getf (class-res (char-class gchar)) type))))
    (+ char-resist class-resist)))

;;; helper function for creating an instance a character
(defun make-character (name char-class level max-hp max-mp str def int agi luk mov crit weapon &key extra resistance)
  (make-instance 'gcharacter :name name :level level :char-class (getf *char-classes* char-class) :max-hp max-hp :hp max-hp :max-mp max-mp :mp max-mp :str str :def def :agi agi :int int :luk luk :mov mov :crit crit :weapon (getf *weapons* weapon) :extra extra :resistance resistance))

;;; test characters
(defparameter *lerris*
  (make-character "Lerris" :fighter 1 12 10 12 8 10 8 10 5 12 :short-sword))

(defparameter *badass*
  (make-character "Chuck Norris" :badass 20 120 70 50 50 50 50 50 5 30 :heron-blade :resistance '(:fire 1.0 :ice 0.5 :lightning 0.25 :poison 1 :sleep 1)))

(setf (ring *badass*) (getf *rings* :power-ring))

;;; test enemies
(defparameter *enemy*
  (make-character "Goblin" nil 4 8 0 10 8 3 7 4 4 7 :short-sword :extra '(:florins 15) :resistance '(:fire 0.25 :physical 0.25)))

;;; item menu action for dropping an item
;;; TODO: figure out if the item remains at that map coordinate to be picked up later
(defun item-drop (item-elt gchar)
  (setf (elt (items gchar) item-elt) nil))

;;; item menu action for giving an item to another character
;;; removes item from owner's list and adds it to first nil elt for the target
;;; returns t if successful, nil if target's list is full
(defun item-give (item-elt owner target)
  (let ((p (position nil (items target))))
    (if p
	(progn
	  (rotatef (elt (items owner) item-elt) (elt (items target) p))
	  t)
	nil)))

;;; item action for giving an item to another character 
;;; switches item in owner's list with one in target's list
;;; primarily for use in Item Give where target's list is full
;;; UI will allow selection of item to switch
(defun item-switch (owner-elt owner target-elt target)
  (rotatef (elt (items owner) owner-elt) (elt (items target) target-elt)))

;;; macro used for defining equip methods for different equipment
(defmacro defequip (item-type)
  (let ((fname (intern (format nil "EQUIP-~A" item-type)))
	(pred-name (intern (format nil "~A-P" item-type))))
    `(defun ,fname (item-elt gchar)
       (let ((item (elt (items gchar) item-elt)))
	 (if (not (,pred-name item))
	     (format t "Item is not a ~A.~%" (quote ,item-type))
	     (progn
	       (rotatef (elt (items gchar) item-elt) (,item-type gchar))))))))

(defequip weapon)
(defequip ring)
(defequip accessory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXPERIENCE AND LEVEL UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; helper method for calculating the level difference between two characters
(defun level-diff (char1 char2)
  (- (level char1) (level char2)))

;;; experience points gained from level difference
(defun level-exp (attacker target)
  (expt 2 (max 0 (+ 3 (level-diff target attacker)))))

;;; experience gained from causing damage to opponent
(defun damage-exp (attacker target damage)
  (let ((dmg-ratio (div damage (max-hp target))))
    (min 48
	 (round (* (level-exp attacker target) dmg-ratio)))))

;;; experience gained from a kill
(defun kill-exp (attacker target damage)
  (min 48 (+ (damage-exp attacker target damage) (level-exp attacker target))))

;;; experience for healing actions (spells or items)
;;; base of 10 + (random 2) if level of target is greater
;;;            + hp restore bonus (NOT IMPLEMENTED)
(defun heal-exp (healer target hp)
  (+ 10 (max (- hp 10) 0)
     (if (> (level target) (level healer))
	 (random 2)
	 0)))

;;; returns experience points gained from one target
;;; handles attacks, kills, heals and support
(defun experience (attacker target damage type)
  (cond
    ((equal type 'attack) (damage-exp attacker target damage))
    ((equal type 'kill) (kill-exp attacker target damage))
    ((equal type 'heal) (heal-exp attacker target damage))
    ((equal type 'support) 10)))

;;; generic functions for characters
(defgeneric melee (gcharacter gcharacter))
(defgeneric melee-atk (gcharacter gcharacter))
(defgeneric apply-damage (gcharacter damage))
(defgeneric melee-damage (gcharacter gcharacter criticalp))
(defgeneric remove-status (status gcharacter))
(defgeneric magic-atk (spell gcharacter gcharacter))
(defgeneric magic-damage (spell gcharacter gcharacter criticalp))
(defgeneric magic-heal (spell gcharacter gcharacter))
(defgeneric apply-healing (gcharacter damage))

(defun print-gchar (char)
  (format t "Name: ~A~%Level: ~A~%HP: ~A~%MP:~A~%Exp: ~A~%"
	  (name char) (level char) (hp char) (mp char) (exp-pts char)))

;;; The melee (weapon) attack method
;;; TODO: receive money from kill, experience points, dropped items
(defmethod melee ((attacker gcharacter) (target gcharacter))
  (progn
    (format t "~A attacks ~A!~%" (name attacker) (name target))
    (melee-atk attacker target)
    (if (critical-p attacker) ; use 'critical' to check for 2nd attack
	(progn
	  (format t "~A attacks again!~%" (name attacker))
	  (melee-atk attacker target)))))

;;; the magic action method
;;; targets must be a list, unlike the melee function which can only
;;; target a single character
(defun magic (spell attacker targets)
  (progn
    (format t "~A casts ~A!~%" (name attacker) (name spell))
    (cast-spell spell attacker targets)))

;;; spell casting wrapper method
;;; decrements magic points of the caster
;;; then applies the spell action to the list of targets
(defun cast-spell (thespell caster targets)
  (progn 
    (decf (mp caster) (mp-use thespell))
    (dolist (target targets)
      (spell-action thespell caster target))))

(defun item-use (item owner target)
  (progn
    (format t "~A uses ~A.~%" (name owner) (name item))
    (item-action item owner target)))

(defun item-heal (item target)
  (format t "~A regains ~A hit points.~%" (name target)
	  (apply-healing target (hp-restore item))))

(defmethod magic-damage (spell (attacker gcharacter) (target gcharacter) criticalp)
  (let* ((mag-diff (- (intelligence-stat attacker) (intelligence-stat target)))
	 (dmg (+ (damage spell) mag-diff))
	 (type (elemental spell)))
    (round (- dmg (* dmg (resistance target type))))))

(defmethod melee-atk ((attacker gcharacter) (target gcharacter))
  (if (atk-hits attacker target)
      (let* ((crit (critical-p attacker))
	     (damage (melee-damage attacker target crit)))
	(progn
	  (format t "~AInflicts ~A points of damage on ~A.~%"
		  (if crit "Critical hit! " "") 
		  (apply-damage target damage) 
		  (name target))
	  damage))
      (format t "~A dodges the attack!~%" (name target))))

(defmethod magic-heal (spell (caster gcharacter) (target gcharacter))
  (progn
    (format t "~A regains ~A hit points!" (name target)
	    (apply-healing target (heal-amount spell caster)))))

(defun heal-amount (spell attacker)
  (+ (hp-restore spell) 
     (if (<= (random 100) (mag-stat attacker)) 
	 (1+ (random 2)) 
	 0)))

(defmethod magic-atk (spell (attacker gcharacter) (target gcharacter))
  (let ((crit (critical-p attacker)))
    (progn
      (format t "~AInflicts ~A points of damage on ~A.~%"
	      (if crit "Critical hit! " "")
	      (apply-damage target (magic-damage spell attacker target crit))
	      (name target)))))

(defmethod apply-healing ((target gcharacter) heal-amt)
  (let ((amount (min heal-amt (- (max-hp target) (hp target)))))
    (progn
      (incf (hp target) amount)
      amount)))

;;; TODO
;;; allow HP absorbing
(defmethod apply-damage ((target gcharacter) damage)
  (let ((amount (min damage (hp target))))
    (progn
      (decf (hp target) amount)
      damage))) ;; return the unadjusted damage for display purposes

;;; determine if physical attack hits target
;;; currently only uses the target's AGI
;;;   needs to account for accuracy of attacker
(defun atk-hits (attacker target)
  (let ((odds (max 1 (min 99 (- 90 (- (agility target) (agility attacker)))))))
    (percentile odds)))

(defmethod melee-damage ((attacker gcharacter) (target gcharacter) criticalp)
  (let ((atk-def-diff (- (strength attacker) (defense target)))
	(type (elemental (weapon attacker))))
    (if (>= 0 atk-def-diff)
	1
	(let ((dmg (round (* (adjust-damage atk-def-diff)
			      (if criticalp *critical-damage* 1)))))
	  (round (- dmg (* dmg (resistance target type))))))))

(defun adjust-damage (dmg)
  (+ (- dmg 1) (random 2))) ; physical attack damage random adjustment

;;; calculate critical using percentage
;;; uses only the overall critical stat of the attacker
(defun critical-p (attacker)
  (percentile (critical attacker)))

;;; calculate counter attack
;;; uses a counter/luck stat
;;; possibly modify by agility difference or level difference
(defun counter-p (attacker)
  (percentile (luck attacker)))

;;; calculate second attack
;;;  it's the minimum of either 25 or the luck of the attacker plus AGI difference
;;;  so the highest possible odds are 1:4.
(defun second-attack-p (attacker target)
  (percentile (min *second-attack-max* (+ (luck attacker) (- (agility attacker) (agility target))))))

;;; A structure for the entire protagonist party
;;; Will contain characters, money, items and anything else needed
(defstruct *party*
  active-members   ; characters in the battle party.  this will be a list.
  inactive-members ; characters not in the battle party.  another list
  florins  ; money.  alternatives: crowns, marks, shekels. 3 2 0 1
  surplus-items ; extra items not able to be carried
  )

;;; UNIT TESTING

(defvar *test-name* nil)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(deftest test-stats ()
  (check
    (= 17 (strength *lerris*))
    (= 12 (critical *lerris*))
    (= 8 (defense *lerris*))
    (= 8 (agility *lerris*))
    (= 10 (luck *lerris*))
    (= 5 (move *lerris*))
    (= 10 (intelligence *lerris*))
    (= 0 (ring-strength *lerris*))
    (= 0 (weapon-agility *lerris*))
    (= 0 (char-class-intelligence *lerris*))))

(deftest test-healing-items ()
  (check
    (equal 10 (hp-restore (getf *healing-items* :herb)))))

(deftest test-weapon ()
  (check
    (equal :physical (elemental (weapon *lerris*)))
    (equal :fire2 (use-action (getf *weapons* :heat-axe)))
    (equal "Fire 2" (name (getf *spells* (use-action (getf *weapons* :heat-axe)))))
))

(deftest test-battle ()
  (let ((dmg (melee-atk *lerris* *enemy*)))
    (check
      (equal dmg (- (max-hp *enemy*) (hp *enemy*))))))

(deftest test-game ()
  (combine-results
    (test-stats)
    (test-weapon)))
