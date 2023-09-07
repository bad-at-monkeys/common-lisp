;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Fucking with different shit man,
;;
;;


(defstruct instrument
  "Refer to https://en.wikipedia.org/wiki/List_of_musical_instruments"
  (instrument nil)
  (classification nil)
  (hs-number nil)
  (origin nil)
  (relation nil))


;;Make a vector with some violin.

(setf violin-vector
      '#(tuning violin 440 a))


;;We should make a violin structure.

(setf violin (make-instrument
              :instrument "Violin"
              :classification "Stringed Instrument"
              :hs-number '(321.322-71)
              :origin "Western Europe"
              :relation nil))


;;Changing the *PRINT-ARRAY* global variable to garnish different return values for our vectors.

(setf *print-array* nil)

(setf *print-array* t)



;;The NTH for arrays.

(aref my-vec 1)



;;Let's make some fresh arrays.

(setf a '#(nil nil nil nil nil))

(setf (aref a 0) 'foo)

(setf (aref a 1) 37)

(setf (aref a 2) 'bar)

(setf (aref a 3) 00)

(setf (aref a 4) 'baz)


;;The same thing but with MAKE-ARRAY.

(make-array 5 :initial-element '(1))
;;=> #((1) (1) (1) (1) (1))

(make-array 5 :intial-contents '(a e i o u))
;;=>#(A E I O U)



;;Changing our Bungholes to Bunsholes.

(setf pet "Bunghole")

(setf (aref pet 3) #\s)

pet



