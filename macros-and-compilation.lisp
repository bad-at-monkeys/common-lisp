;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  14 Macros and Compilation
;;  
;;


;;Run ppmx.lisp

(ppmx (incf p))


;;INCF cleverly determining what a place description means.

(incf (aref (nth array-num *list-of-arrays*)
            (first subscripts)))


;;PPMX on POP

;; CL-USER> (ppmx (pop x))
;; First step of expansion:
;; (PROG1 (CAR X) (SETQ X (CDR X)))

;; Final expansion:
;; (LET ((#:G704 (CAR X)))
;;   (SETQ X (CDR X))
;;   #:G704)



;; CL-USER> (ppmx (defstruct starship
;;                  (name nil)
;;                  (condition 'green)))
;; Macro expansion:
;; (PROGN
;;   (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;     (SB-KERNEL::%DEFSTRUCT-PACKAGE-LOCKS
;;      '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {1004E630C3}>))
;;   (SB-KERNEL::%DEFSTRUCT
;;    '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {1004E630C3}>
;;    '#(#<SB-KERNEL:LAYOUT for T {10000733C3}>
;;       #<SB-KERNEL:LAYOUT for STRUCTURE-OBJECT {1000073443}>)
;;    (SB-C:SOURCE-LOCATION))
;;   (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;     (SB-KERNEL::%COMPILER-DEFSTRUCT
;;      '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {1004E630C3}>
;;      '#(#<SB-KERNEL:LAYOUT for T {10000733C3}>
;;         #<SB-KERNEL:LAYOUT for STRUCTURE-OBJECT {1000073443}>)))
;;   (SB-C:XDEFUN COPY-STARSHIP
;;       :COPIER
;;       (SB-KERNEL:INSTANCE)
;;     (COPY-STRUCTURE (THE STARSHIP SB-KERNEL:INSTANCE)))
;;   (SB-C:XDEFUN STARSHIP-P
;;       :PREDICATE
;;       (SB-KERNEL::OBJECT)
;;     (TYPEP SB-KERNEL::OBJECT 'STARSHIP))
;;   (SB-C:XDEFUN (SETF STARSHIP-NAME)
;;       :ACCESSOR
;;       (SB-KERNEL::VALUE SB-KERNEL:INSTANCE)
;;     (SB-KERNEL:%INSTANCE-SET (THE STARSHIP SB-KERNEL:INSTANCE) 1
;;                              SB-KERNEL::VALUE))
;;   (SB-C:XDEFUN STARSHIP-NAME
;;       :ACCESSOR
;;       (SB-KERNEL:INSTANCE)
;;     (SB-KERNEL:%INSTANCE-REF (THE STARSHIP SB-KERNEL:INSTANCE) 1))
;;   (SB-C:XDEFUN (SETF STARSHIP-CONDITION)
;;       :ACCESSOR
;;       (SB-KERNEL::VALUE SB-KERNEL:INSTANCE)
;;     (SB-KERNEL:%INSTANCE-SET (THE STARSHIP SB-KERNEL:INSTANCE) 2
;;                              SB-KERNEL::VALUE))
;;   (SB-C:XDEFUN STARSHIP-CONDITION
;;       :ACCESSOR
;;       (SB-KERNEL:INSTANCE)
;;     (SB-KERNEL:%INSTANCE-REF (THE STARSHIP SB-KERNEL:INSTANCE) 2))
;;   (SB-C:XDEFUN MAKE-STARSHIP
;;       :CONSTRUCTOR
;;       (&KEY ((:NAME #:NAME) NIL)
;;             ((:CONDITION #:CONDITION)
;;              (SB-C::WITH-SOURCE-FORM (CONDITION 'GREEN) 'GREEN)))
;;     (DECLARE (SB-INT:EXPLICIT-CHECK)
;;              (SB-C::LAMBDA-LIST
;;               (&KEY ((:NAME #:NAME) NIL)
;;                     ((:CONDITION #:CONDITION) (QUOTE GREEN)))))
;;     (SB-KERNEL::%MAKE-STRUCTURE-INSTANCE-MACRO
;;      #<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {1004E630C3}>
;;      '((:SLOT T . 1) (:SLOT T . 2)) #:NAME #:CONDITION))
;;   (SB-KERNEL::%TARGET-DEFSTRUCT
;;    '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {1004E630C3}>))

;;                                         ; No value


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  14.4 DEFINING A MACRO
;;
;;

;;Make a simple INCF.

(defmacro simp-incf (var)
  (list 'setq var (list '+ var 1)))

(setf a 4)

;; CL-USER> (simp-incf a)
;; 5

;;We can see that our PPMX returns a simplified version of the macro definition.

;; CL-USER> (ppmx (simp-incf a))
;; Macro expansion:
;; (SETQ A (+ A 1))

;; ; No value

;;Re-define SIMP-INCF, specify an incremental value with an &OPTIONAL.

(defmacro simp-incf (var &optional (amount 1))
  (list 'setq var (list '+ var amount)))

(setf a 5)

(setf b 2)

;; CL-USER> (ppmx (simp-incf b (* 3 a)))
;; Macro expansion:
;; (SETQ B (+ B (* 3 A)))

;; ; No value
;; CL-USER> b
;; 2
;; CL-USER> (simp-incf b (* 3 a))
;; 17
;; CL-USER> b
;; 17


;;Defining INCF with DEFUN instead of DEF MACRO.

(defun bad-incf (var)
  (setq var (+ var 1)))

;; CL-USER> (defun bad-incf (var)
;;            (setq var (+ var 1)))
;; BAD-INCF
;; CL-USER> (bad-incf a)
;; 6
;; CL-USER> a
;; 5
;; CL-USER> (bad-incf a)
;; 6
;; CL-USER> a
;; 5


;;Write a macro that sets a variable to NIL.

(defmacro set-nil (var)
  (list 'setq var ()))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  14.6 THE BACKQUOTE CHARACTER
;;
;;

(setf name 'fred)

'(this is ,name from pittsburgh)

'(i gave ,name about ,(* 25 8) dollars)

;;We can use the backquote to write a more concise version of SIMP-INCF

(defmacro simp-incf (var &optional (amount 1))
  '(setq ,var (+ ,var ,amount)))

(ppmx (simp-incf fred-loan (* 25 8)))


;;Write a macro that switches values between two variables.

(defmacro switch-values (var1 var2)
  `(let ((temp1 ,var2)
         (temp2 ,var1))
     (setq ,var1 temp2)
     (setq ,var2 temp1)))


;;Using backquote to generate expressions with quotes.

`(setf foo 'bar)    ;=> (setf foo 'bar)


(defmacro deja-two (func obj)
  `(,func ',obj ',obj))


;;Write SET-MUTUAL, which sets two variable names to one another.

(defmacro set-mutual (var1 var2)
  `(progn
     (setf ,var1 ',var2)
     (setf ,var2 ',var1)))


;;Write a function that displays the value of a variable.

(defun f (x y)
  (showvar x)
  (showvar y)
  (* x y))


(defmacro showvar (var)
  `(format t "~&The value of ~S is ~S"
           ',var
           ,var))


;;Splicing with ,@

(setf producer '(Ricky-Golfarts))

(setf band-name '(The-Dingle-Brothers))

(setf occupation '(record-producer))

;; CL-USER> `(,producer is the ,occupation that put platinum hit band ,band-name on the map.)
;; (RICKY-GOLFARTS IS THE (RECORD-PRODUCER) THAT PUT PLATINUM HIT BAND
;;                 THE-DINGLE-BROTHERS ON THE MAP.)


;;If we follow our , with an @ we can splice our variables in nicely.

`(,@producer is the ,@occupation that put platinum hit band ,@band-name on the map.)

;; CL-USER> `(,@producer is the ,@occupation that put platinum hit band ,@band-name on the map.)
;; (RICKY-GOLFARTS IS THE RECORD-PRODUCER THAT PUT PLATINUM HIT BAND
;;   THE-DINGLE-BROTHERS ON THE MAP.)


;;Splicing is useful when setting multiple variable to a value.

(reset-counts t1 t2 t3)


(defmacro reset-counts (&rest variables)
  `(progn ,@(mapcar #'(lambda (var)
                        (list 'setf var 0))
                    variables)
          '(Scores for ,@variables have been reset.)))

(ppmx (reset-counts t1 t2 t3))


;; CL-USER> (ppmx (reset-counts t1 t2 t3))
;; Macro expansion:
;; (PROGN
;;   (SETF T1 0)
;;   (SETF T2 0)
;;   (SETF T3 0)
;;   '(SCORES FOR T1 T2 T3 HAS BEEN RESET.))

;;                                         ; No value


;;Accept any number of inputs and chain them together.

(defmacro variable-chain (&rest vars)
  `(progn
     ,@(do ((v vars (rest v))
            (res nil))
           ((null (rest v)) (reverse res))
         (push `(setf ,(first v)
                 ',(second v))
               res))))


;; CL-USER> (list a b c d)
;; (A B C D)
;; CL-USER> (variable-chain a b c d)
;; D
;; CL-USER> (list a b c d)
;; (B C D D)


;; CL-USER> (ppmx (variable-chain a b c d))
;; Macro expansion:
;; (PROGN (SETF A 'B) (SETF B 'C) (SETF C 'D))

;; ; No value


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  14.8 THE COMPILER
;;
;;


;;Return the smallest integer larger than the square root of its input.

(defun tedious-sqrt (n)
  (dotimes (i n)
    (if (> (* i i) n) (return i))))

;; CL-USER> (time (tedious-sqrt 999999999))
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;; 100.00% CPU
;; 504,368 processor cycles
;; 0 bytes consed

;; 31623
;; CL-USER> (compile 'tedious-sqrt)
;; TEDIOUS-SQRT
;; NIL
;; NIL
;; CL-USER> (time (tedious-sqrt 999999999))
;; Evaluation took:
;; 0.000 seconds of real time
;; 0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;; 100.00% CPU
;; 503,715 processor cycles
;; 0 bytes consed

;; 31623


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 14.9 COMPILATION AND MACRO EXPANSION
;;
;;

;;Bad macro.

(defmacro bad-announce-macro ()
  (format t "~%Hi mom!"))

(defun say-hi ()
  (bad-announce-macro))

;; CL-USER> (compile 'say-hi)

;; SAY-HI
;; NIL
;; NIL
;; CL-USER> (say-hi)
;; NIL

;;We fix the issue by defining the macro to return the expression, not the value.

(defmacro good-announce-macro ()
  (format t "~%Hi mom!"))


