;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Hash Tables and Property Lists
;;
;;


(setf hhh (make-hash-table))
;;#<HASH-TABLE :TEST EQL :COUNT 0 {1005089A33}>

(type-of hhh)
;;HASH-TABLE


;;Getting hashes in a way.

(setf (gethash 'john hhh)
      '(attorney (16 maple drive)))


(setf (gethash 'mary hhh)
      '(physician (23 cedar court)))

(gethash 'john hhh)

(gethash 'bill hhh)

hhh
;;#<HASH-TABLE :TEST EQL :COUNT 2 {1005089A33}>


;;Playing around with property lists.

(ind-1 value-1 ind-2 value-2 ..)    ;Property lists are organized in this way.


(setf (get 'fred 'sex) 'male)

(setf (get 'fred 'age) 23)

(setf (get 'fred 'siblings) '(george wanda))

;;=> (siblings (george wanda) age 23 sex male)


(get 'fred 'age)


(incf (get 'fred 'age))

(get 'fred 'age)


(symbol-plist 'fred)


(remprop 'fred 'age)

(get 'fred 'age)



;;Programming with property lists.

(defun addprop (sym e prop)
  (pushnew e (get sym prop)))


(defun record-meeting (x y)
  (addprop x y 'has-met)
  (addprop y x 'has-met)
  t)



;;Make a hash table.

(setf fleet (make-hash-table))

(setf (gethash 'knarr fleet)
      '(norse-merchant-ship (skuldelev-1)))

(setf (gethash 'cog fleet)
      '(frisian-trading-vessel (roland-von-bremen)))

(setf (gethash 'caravel fleet)
      '(portuguese-exploration-ship (boa-esperanca)))

(setf (gethash 'birlinn fleet)
      '(west-highland-galley (aileach)))



;;Exercises 13
;;Write SUBPROP, deleting an element from a set stored under a property name.

(defun subprop (symbol item property)
  (setf (get symbol property)
        (remove item (get symbol property))))

;;Recall ADDPROP.

(defun addprop (symbol element property)
  (pushnew element (get symbol property)))


;;I have gotten poop on my bum.

(setf (get 'bum 'poop) 'poop-on-my-bum)

(get 'bum 'poop)



;;This is the story of the pirate Captain Willy Gavelston battling the Navy Bungs in 1647.

(setf (get 'pirate-ship 'captain) 'willy-gavelston)

(setf (get 'pirate-ship 'crew) 25)

(setf (get 'pirate-ship 'name) 'bloody-kisses)

(setf (get 'pirate-ship 'cannons) 5)

(setf (get 'pirate-ship 'loot) 'gold-and-silver)


(defun has-battled (ship-1 ship-2)
  (addprop ship-1 ship-2 'has-battled)
  (addprop ship-2 ship-1 'has-battled)
  t)


(setf (get 'naval-vessel 'admiral) 'deushenbag)

(setf (get 'naval-vessel 'crew) 50)

(setf (get 'naval-vessel 'name) 'sandusky)

(setf (get 'naval-vessel 'cannons) 15)

(setf (get 'naval-vessel 'siezed-contraband) '(gems))


(defun forget-battle (ship-1 ship-2)
  (subprop ship-1 ship-2 'has-battled)
  (subprop ship-2 ship-1 'has-battled)
  'history-erased)


;;Write your own version of GET using SYMBOL-PLIST.

(defun get-property-value (symbol property)
  (do ((prop (symbol-plist symbol) (cddr prop)))
      ((null prop) nil)
    (if (equal property (first prop))
        (return (second prop)))))


;;Testing for particular properties.

(defun has-property (symbol property)
  (do ((p (symbol-plist symbol) (cddr p)))
      ((null p) nil)
    (if (equal property (first p))
        (return t))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Array Keyboard Exercise
;;
;;


;;Hold the array of counts.

(setf *hist-array* nil)


;;Hold number of points thus far.

(setf *total-points* 0)


;;Write the initialization function taking the number of total buckets as input.

(defun new-histogram (bins)
  (setf *total-points* 0)
  (setf *hist-array*
        (make-array bins :initial-element 0))
  t)



;;Record and update *TOTAL-POINTS*.

(defun record-value (v)
  (incf *total-points*)
  (if (and (>= v 0)
           (< v (length *hist-array*)))
      (incf (aref  *hist-array* v))
      (error "Value ~S out of bounds." v)))


(defun print-hist-line (i)
  (let ((val (aref *hist-array* i)))
    (format t "~&~2D [~3D] " i val)
    (dotimes (j val)
      (format t "*"))))


;;Write PRINT-HISTOGRAM

(defun print-histogram ()
  (dotimes (i (length *hist-array*))
    (print-hist-line i))
  (format t "~&    ~3D total" *total-points*))


;;Here is how the program would be used.

(new-histogram 11)


(dotimes (i 200)
  (record-value (random 11)))


(print-histogram)
