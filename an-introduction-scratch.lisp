;;;; Welcome to Portacle, the Portable Common Lisp Environment.
;; For information on Portacle and how to use it, please read
;;   https://portacle.github.io or *portacle-help*
;; To report problems and to get help with issues,  please visit
;;   https://github.com/portacle/portacle/issues
;; Portacle is currently running SLIME , but you can
;;   Switch to SLY
;; You should also configure Portacle with the
;;   First-time setup
;; 
;; You can use this buffer for notes and tinkering with code.


(defun my-square ()
  "Prompts for a number, returns a string stating the square of it."
  (format t "Please type in a number: ")
  (let ((x (read)))
    (format t "The number ~S squared is ~S.~%"
            x (* x x))))

(defun gross-pay ()
  "Takes an hourly wage in dollars and number of hours worked."
  (format t "~&Enter your hourly wage: ")
  (let ((wage (read)))
    (format t "~&Enter hours worked: ")
    (let ((hours (read)))
      (format t "~&"))))

(defun compensation ()
  "Computes the compensation of an hourly worker. Prompts name, wage, hours worked."
  (format t "~&Name of employee: ")
  (let ((name (read)))
    (format t "~&Employee hourly wage: ")
    (let ((wage (read)))
      (format t "~&Employee hours worked: ")
      (let ((hours (read)))
        (format t "~&~S earned ~S dollars."
                name (* wage hours))))))

(defun cookie-monster ()
  "It only wants a cookie."
  (format t "Give me cookie!!!~%Cookie? ")
  (let ((response (read)))
    (cond ((equal response 'cookie)
           (format t "~&Thank you!")
           (format t "...Munch, munch, munch")
           (format t "... BURP"))
          (t (format t "~&No want ~S...~%~%"
                     response)
             (cookie-monster)))))

(defun riddle ()
  (if (yes-or-no-p
       "Do you seek Zen enlightenment? ")
      (format t "Then do not ask for it!")
      (format t "You have found it.")))

(defun get-tree-data ()
  "Gets the tree data from timber.dat"
  (with-open-file (stream "c:/Users/tylerbrw/Documents/lisp/references/an-introduction/timber.dat")
    (let* ((tree-loc (read stream))
           (tree-stage (read stream))
           (tree-table (read stream))
           (tree-total (read stream)))
      (format t "~&There are ~S trees on ~S."
              tree-total tree-loc)
      (format t "~&Tree types include: ~S"
              tree-table)
      (format t "~&Growth stage group: ~S"
              tree-stage))))

(defun save-tree-data (tree-loc tree-stage
                       tree-table tree-total)
  "Takes the tree data, streams the data as output to the timber.newdat file."
  (with-open-file (stream "c:/Users/tylerbrw/Documents/lisp/references/an-introduction/timber.newdat"
                          :direction :output)
    (format stream "~S~%" tree-loc)
    (format stream "~S~%" tree-stage)
    (format stream "~S~%" tree-table)
    (format stream "~S~%" tree-total)))

(defun space-over (n)
  "Takes a number and moves the cursor to the right by orinting N spaces."
  (cond ((plusp n)
         (format t " ")
         (space-over (- n 1)))
        ((zerop n) nil)
        (t (format t "Error!"))))

(defun test (n)
  "Testing function for the Chapter 9 Keyboard Exercise."
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))

(defun plot-one-point (plotting-string y-val)
  "Plots one point on the input coordinates. Moves to a new line after."
  (space-over y-val)
  (format t "~A~%" plotting-string))

(defun plot-points (plotting-string y-vals)
  "Plots the provided points that many times over for each y-value."
  (mapcar #'(lambda (y)
              (plot-one-point plotting-string y))
          y-vals))

(defun generate (m n)
  "Generates a list of numbers M through N."
  (cond ((equal m n) (list n))
        (t (cons m (generate (+ m 1) n)))))

(defun make-graph ()
  "Creates the graph dude .."
  (let* ((func
           (format t "~&Graphing function: "))
         (start
           (format t "~&Starting-x: "))
         (end (format t "~&Ending-x: "))
         (plotting-string
           (format t "~&Plotting string: ")))
    (plot-points plotting-string
                 (mapcar func (generate start end)))
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   Make Graph: The Remix
;;     Tyler Brandon Wright
;;
;;
;;
;;
;;


(defun space-over (n)
  "Moves point N-spaces to the right."
  (cond ((plusp n)
         (format t " ")
         (space-over (- n 1)))
        ((zerop n) nil)
        (t (format t "Error!"))))

(defun test (n)
  "Testing function for space over."
  (format t "~%>>>")
  (space-over n)
  (format t "<<<"))

(defun test-two (n)
  "TEST without the ~% directive."
  (format t ">>>")
  (space-over n)
  (format t "<<<"))

(defun test-three (n)
  "TEST with only the right-side of the return value."
  (space-over n)
  (format t " <<<"))

(defun plot-one-point (plotting-string y-val)
  "Plots a string on a list of y-axes. Each y-axis listed creates a new line."
  (space-over y-val)
  (format t "~A~%" plotting-string))

(defun plotting-string ()
  "String to be plotted in PLOT-ONE-POINT."
  (format t "PLOTTING-STRING"))

;; Come back to this when we have a better understanding of Common Lisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Advanced Topics
;;    9
;;      Tyler Brandon
;;
;;
;;

;; 9.8 PARAMETERS TO FORMAT DIRECTIVES

(setf glee-club
      '((john smith) (barbara wilson)
        (bill clinton) (barrack obama)
        (ali mustapha) (shalala lalabalala)
        (tyler brandon) (shepherd axel)))

(defun print-one-name (name)
  "Prints the SECOND then FIRST top level elements. NAMEs in this case."
  (format t "~&~10S ~S"
          (second name)
          (first name)))

(defun print-all-names (alist)
  "Prints all top-level elements SECOND, FIRST; columnar formatting 10 spaces width."
  (mapcar #'print-one-name alist)
  'done)


;; 9.9 ADDITIONAL FORMAT DIRECTIVES

(defun sevenths (nums)
  "Takes NUMS, translates them into a 7-character field with 5 decimal spaces (base-10)."
  (mapcar #'(lambda (numerator)
              (format t "~&~4,2F / 7 is ~7,5F"
                      numerator
                      (/ numerator 7.0)))
          nums)
  'done)


;; 9.10 THE LISP 1.5 OUTPUT PRIMITIVES

(defun my-print (list)
  "User-defined version of PRINT."
  (terpri)
  (prin1 list)
  (princ " ")
  list)

(defun bogosity ()
  "Blanket of bogusness."
  (mapcar #'my-print '(b o g o s i t y)))


;; 9.11 HANDLING END-OF-FILE CONDITIONS

(defun read-my-file ()
  "Reads an arbitrary file of Lisp objects, tells how many were read, then returns them."
  (with-open-file (stream "c:/Users/tylerbrw/Documents/lisp/references/an-introduction/bung/bogo-file.lisp")
    (let ((contents
            (read-all-objects stream (list '$eof$))))
      (format t "~&Read ~S objects from the file."
              (length contents))
      contents)))

(defun read-all-objects (stream eof-indicator)
  "Reads all of the Lisp objects within an arbitrary file."
  (let ((result (read stream nil eof-indicator)))
    (if (eq result eof-indicator)
        nil
        (cons result (read-all-objects stream eof-indicator)))))


;; 9.12 PRINTING IN DOT NOTATION

(defun dot-prin1 (lst)
  "Takes a standard list and returns it in dot notation."
  (cond ((null lst) nil)
        ((atom lst) (format t "~S" lst))
        (t (format t "(")
           (dot-prin1 (car lst))
           (format t " . ")
           (dot-prin1 (cdr lst))
           (format t ")"))))

(defun dot-prin1 (lst)
  "Takes a standard list and returns it in dot notation."
  (cond ((atom lst) (format t "~S" lst))
        (t (format t "(")
           (dot-prin1 (car lst))
           (format t " . ")
           (dot-prin1 (cdr lst))
           (format t ")"))))


;; 9.13 HYBRID NOTATION

(defun hybrid-prin1 (lst)
  "Determines whether to return the input in dot or list notation. Returns the determination."
  (cond ((atom lst) (format t "~S" lst))
        (t (hybrid-print-car (car lst))
           (hybrid-print-cdr (cdr lst)))))

(defun hybrid-print-car (lst)
  "Helper function to HYBRID-PRIN1."
  (format t "(")
  (hybrid-prin1 lst))

(defun hybrid-print-cdr (lst)
  "Helper function to HYBRID-PRIN1."
  (cond ((null lst) (format t ")"))
        ((atom lst) (format t " . ~S)" lst))
        (t (format t " ")
           (hybrid-prin1 (car lst))
           (hybrid-print-cdr (cdr lst)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    10 Assignment
;;
;;
;;
;;
;;

;; 10.2 UPDATING A GLOBAL VARIABLE

(defun sell-old (n)
  "Ye Olde Lemonade Stand: Sales by the Glass."
  (setf *total-glasses* (+ *total-glasses* n))
  (format t "~&Glasses sold so far: ~S"
          *total-glasses*))

(defun sell (n)
  "The New lemonade stand bitches."
  (incf *total-glasses* n)
  (format t "~&Glasses sold so far: ~S"
          *total-glasses*))


;; 10.3.2 The PUSH and POP macros

(pop my-stack)

(let ((top-element (first my-stack)))
  (setf my-stack (rest my-stack))
  top-element)


(setf *friends* nil)

(defun meet-old (person)
  "Who the fuck ..?"
  (cond ((equal person (first *friends*))
         'we-just-met)
        ((member person *friends*)
         'we-know-eachother)
        (t (push person *friends*)
           'pleased-to-meet-you)))


(setf *met-before* 0)

(defun meet (person)
  "Hi, nice to meet ya dude. I think I've met some of y'all before .."
  (cond ((equal person (first *friends*))
         (incf *met-before*)
         'we-just-met)
        ((member person *friends*)
         (incf *met-before*)
         'we-know-eachother)
        (t (push person *friends*)
           'pleased-to-meet-you)))

(defun forget-you (person)
  "Fuck off ya bloody cunt."
  (cond ((member person *friends*)
         (setf *friends*
               (remove person *friends*))
         'forgotten)
        (t (list 'dont 'know 'dude))))


(setf *homies* nil)

(defun makin-homies (dude)
  "Wuddup dude?"
  (cond ((equal dude (first *homies*))
         'we-literally-just-met-dude)
        ((member dude *homies*)
         'we-are-homies-dude)
        (t (push dude *homies*)
           'welcome-dude)))

(setf *already-met-these-dudes* 0)

(defun makin-homies-efficiently (dude)
  "Duuuuude, this is better."
  (cond ((equal dude (first *homies*))
         (incf *already-met-these-dudes*)
         'we-literally-just-met-dude)
        ((member dude *homies)
         (incf *already-met-these-dudes*)
         'we-are-homies-dude)
        (t (push dude *homies*)
           'welcome-dude)))

(defun forget-you-dude (dude)
  "We ain't dudes anymore dude."
  (cond ((member dude *homies*)
         (setf *homies*
               (remove dude *homies*))
         'forget-you-dude)
        (t (list 'dont 'know 'this 'dude))))



;; 10.3.3 Updating Local Variables

(defun bad-style (n)
  (format t "~&N is ~S." n)
  (decf n 2)
  (format t "~&Now N is ~S." n)
  (decf n 2)
  (format t "~&Now N is ~S." n)
  (list 'result 'is (* n (- n 1))))

(defun good-style (n)
  (let* ((p (- n 2))
         (q (- p 2)))
    (format t "~&N is ~S." n)
    (format t "~&P is ~S." p)
    (format t "~&Q is ~S." q)
    (list 'result 'is (* q (- q 1)))))


(defun get-name ()
  "Into the system you go."
  (let ((last-name nil)
        (first-name nil)
        (middle-initial nil)
        (preferred-title nil))
    (format t "~&Input Last Name : ")
    (setf last-name (read))
    (format t "~&First Name: ")
    (setf first-name (read))
    (format t "~&Middle Initial: ")
    (setf middle-initial (read))
    (format t "~&Preferred Title: ")
    (setf preferred-title (read))
    (list preffered-title first-name middle-initial last-name)))



;; 10.4 WHEN AND UNLESS

(defun picky-times (x y)
  (unless (oddp x)
    (incf x)
    (format t "~&Changing X to ~S to make it odd." x))
  (when (oddp y)
    (decf y)
    (format t "~&Changing Y to ~S to make it even." y))
  (* x y))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  10.6 CASE STUDY: TIC-TAC-TOE PLAYER
;;
;;
;;
;;
;;


;;Makes the board.

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))


;;Assigning board creation to variable B.

(setf b (make-board))

(nth 1 b)

(nth 0 b)


;;Building a conversion tool and pieces to the board printer.

(defun convert-to-letter (v)
  (cond ((equal v 1) "O")
        ((equal v 10) "X")
        (t " ")))

(defun print-row (x y z)
  (format t "~&  ~A | ~A | ~A"
          (convert-to-letter x)
          (convert-to-letter y)
          (convert-to-letter z)))

(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~&  ---------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~&  ---------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))

(print-board b)


;;Make moves on the board.

(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)


;;Defining two players; COMPUTER X, OPPONENT O for clarity.

(setf *computer* 10)

(setf *opponent* 1)

(make-move *opponent* 3 b)

(make-move *computer* 5 b)


;;Storing all eight possible winning triplets.

(setf *triplets*
      '((1 2 3) (4 5 6) (7 8 9)    ;Horizontal triplets.
        (1 4 7) (2 5 8) (3 6 9)    ;Vertical triplets.
        (1 5 9) (3 5 7)))          ;Diagonal triplets.


;;Computing the sum of the triplets, determining what move is where.

(defun sum-triplet (board triplet)
  (+ (nth (first triplet) board)
     (nth (second triplet) board)
     (nth (third triplet) board)))

(sum-triplet b '(3 5 7))    ;Left-diagonal.

(sum-triplet b '(2 5 8))    ;Middle-vertical.

(sum-triplet b '(7 8 9))    ;Bottom horizontal.


;;Writing a function that returns a list of all eight sums, as moves are marked on the board.

(defun compute-sums (board)
  (mapcar #'(lambda (triplet)
              (sum-triplet board triplet))
          *triplets*))


;;Predicate testing for the winning condition.

(defun winner-p (board)
  (let ((sums (compute-sums board)))
    (or (member (* 3 *computer*) sums)
        (member (* 3 *opponent*) sums))))

;;Game initialization.

(defun play-one-game ()
  (if (y-or-n-p "Would you like to go first? ")
      (opponent-move (make-board))
      (computer-move (make-board))))

;;Prompting for opponent move choice and checking move legality.

(defun opponent-move (board)
  (let* ((pos (read-a-legal-move board))
         (new-board (make-move
                     *opponent*
                     pos
                     board)))
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&You win."))
          ((board-full-p new-board)
           (format t "~&Tie. Both y'all lose."))
          (t (computer-move new-board)))))


;;Function testing for move legality. (recursive)

(defun read-a-legal-move (board)
  (format t "~&Your move: ")
  (let ((pos (read)))
    (cond ((not (and (integerp pos)
                     (<= 1 pos 9)))
           (format t "~&Invalid input.")
           (read-a-legal-move board))
          ((not (zerop (nth pos board)))
           (format t
                   "~&That space is already occupied.")
           (read-a-legal-move board))
          (t pos))))

;;Tests whether there are any open spaces.

(defun board-full-p (board)
  (not (member 0 board)))


;;Parameters for the computer move.

(defun computer-move (board)
  (let* ((best-move (choose-best-move board))
         (pos (first best-move))
         (strategy (second best-move))
         (new-board (make-move
                     *computer* pos board)))
    (format t "~&My move: ~S" pos)
    (format t "~&My strategy: ~A~%" strategy)
    (print-board new-board)
    (cond ((winner-p new-board)
           (format t "~&I win!"))
          ((board-full-p new-board)
           (format t "~&Tie game."))
          (t (opponent-move new-board)))))


;;One strategy: pick a legal move at random.

(defun choose-best-move (board)         ;First version.
  (random-move-strategy board))

;;The move and an explanation of the logic behind it.

(defun random-move-strategy (board)
  (list (pick-random-empty-position board)
        "random move"))

;;Picking a random number from 1 to 9 for position.

(defun pick-random-empty-position (board)
  (let ((pos (+ 1 (random 9))))
    (if (zerop (nth pos board))
        pos
        (pick-random-empty-position board))))

;;Adding intelligence to our system.

(defun make-three-in-a-row (board)
  (let ((pos (win-or-block board
                           (* 2 *computer*))))
    (and pos (list pos "make three in a row"))))

(defun block-opponent-win (board)
  (let ((pos (win-or-block board
                           (* 2 *opponent*))))
    (and pos (list pos "block opponent"))))

(defun win-or-block (board target-sum)
  (let ((triplet (find-if
                  #'(lambda (trip)
                      (equal (sum-triplet board
                                          trip)
                             target-sum))
                  *triplets*)))
    (when triplet
      (find-empty-position board triplet))))

(defun find-empty-position (board squares)
  (find-if #'(lambda (pos)
               (zerop (nth pos board)))
           squares))

;;Attempting evaluation of each strategy one at a time until one works.

(defun choose-best-move (board)    ;Second version.
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (random-move-strategy board)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Chapter 10 Review Exercises
;;
;;
;;

;;Rewrite this with good style.

(defun ugly (x y)
  (when (> x y)
    (setf temp y)
    (setf y x)
    (setf x temp))
  (setf avg (/ (+ x y) 2.0))
  (setf pct (* 100 (/ avg y)))
  (list 'average avg 'is
        pct 'percent 'of 'max y))

;;Elegantly.

(defun pretty (x y)
  (let* ((biggest (max x y))
         (smallest (min x y))
         (avg (/ (+ x y) 2.0))
         (pct (* 100 (/ avg biggest))))
    (list 'average avg 'is pct 'percent
          'of 'max biggest)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CHAPTER 10  Assignment
;;    Lisp Toolkit: BREAK and ERROR
;;
;;

(defun analyze-profit (price commission-rate)
  (let* ((commission (* price commission-rate))
         (result
           (cond ((> commission 100) 'rich)
                 ((< commission 100) 'poor))))
    (format t "~&I predict you will be: ~S"
            result)
    result))

(analyze-profit 2000 0.05)    ;Returns NIL. Needs debugging.


;;Inserting BREAK into ANAlYZE-PROFIT for debugging.

(defun analyze-profit (price commission-rate)
  (let* ((commission (* price commission-rate))
         (result
           (cond ((> commission 100) 'rich)
                 ((< commission 100) 'poor))))
    (break "Value of RESULT is ~S" result)
    (format t "~&I predict you will be: ~S"
            result)
    result))

;;Putting a T there.

(defun analyze-profit (price commission-rate)
  (let* ((commission (* price commission-rate))
         (result
           (cond ((> commission 100) 'rich)
                 (t (< commission 100) 'poor))))
    (break "Value of RESULT is ~S" result)
    (format t "~&I predict you will be: ~S"
            result)
    result))

;;Checking for correct input.

(defun average (x y)
  (unless (and (numberp x) (numberp y))
    (error "Arguments must be numbers:  ~S, ~S"
           x y))
  (/ (+ x y) 2.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Keyboard Exercise
;;    Chapter 10
;;      Assignment
;;
;;
;;  Extending Tic-Tac-Toe
;;
;;


;;We can extend the tic-tac-toe program by adding to its available strategy pool
;;in CHOOSE-BEST-MOVE. The additional strategies are the two-on-one and the squeeze play.

;;We are told to initialize the program with the second version of CHOOSE-BEST-MOVE.

(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (random-move-strategy board)))


;;Hold a list of the four corner positions on the tic-tac-toe game board.

(setf *corners* '(1 3 7 9))


;;Hold a list of the four sides on the tic-tac-toe game board.

(setf *sides* '(2 4 6 8))


;;Blocking the squeeze play.

(defun block-squeeze-play (board)
  (sq-and-2 board *computer* *sides* 12
            "block squeeze play"))


;;Blocking the two-on-one.

(defun block-two-on-one (board)
  (sq-and-2 board *opponent* *corners* 12
            "block two-on-one"))



;;Employing the strategies offensively.

(defun try-squeeze-play (board)
  (sq-and-2 board *opponent* nil 11
            "set up a squeeze play"))


(defun try-two-on-one (board)
  (sq-and-2 board *opponent* nil 11
            "set up a two-on-one"))


(defun sq-and-2 (board player pool v strategy)
  (when (equal (nth 5 board) player)
    (or (sq-helper board 1 9 v strategy pool)
        (sq-helper board 3 7 v strategy pool))))


(defun sq-helper (board c1 c2 val strategy pool)
  (when (equal val (sum-triplet
                    board (list c1 5 c2)))
    (let ((pos (find-empty-position
                board
                (or pool (list c1 c2)))))
      (and pos (list pos strategy)))))


(defun exploit-two-on-one (board)
  (when (equal (nth 5 board) *computer*)
    (or (exploit-two board 1 2 4 3 7)
        (exploit-two board 3 2 6 1 9)
        (exploit-two board 7 4 8 1 9)
        (exploit-two board 9 6 8 3 7))))


(defun exploit-two (board pos d1 d2 c1 c2)
  (and (equal (sum-triplet
               board
               (list c1 5 c2)) 21)
       (zerop (nth pos board))
       (zerop (nth d1 board))
       (zerop (nth d2 board))
       (list pos "exploit two-on-one")))


(defun choose-best-move (board)
  (or (make-three-in-a-row board)
      (block-opponent-win board)
      (block-squeeze-play board)
      (block-two-on-one board)
      (exploit-two-on-one board)
      (try-squeeze-play board)
      (try-two-on-one board)
      (random-move-strategy board)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  10 Advanced Topics
;;  
;;

;;10.7 DO-IT-YOURSELF LIST SURGERY

;;Building a snipper

(defun snip (lst)
  (setf (cdr lst) (cdr (cdr lst))))


(setf a '(no down payment))


(setf b (cdr a))


(snip a)

a

b

(setf a '(no down payment))

(setf b (cdr a))


;;10.8 DESTRUCTIVE OPERATIONS ON LISTS

;;Defining our own NCONC

(defun my-nconc (arg-1 arg-2)
  (cond ((null arg-1) arg-2)
        (t (setf (cdr (last arg-1)) arg-2)
           arg-1)))


;;10.9 PROGRAMMING WITH DESTRUCTIVE OPERATIONS

(setf *things* '((object1 large green shiny cube)
                 (object2 small red dull metal cube)
                 (object3 red small dull plastic cube)))


;;A general function that renames objects.

(defun rename-element (obj newname)
  (setf (car (assoc obj *things*)) newname))


;;Adding a new property to an object with NCONC.

(defun add-property (obj prop)
  (nconc (assoc obj *things*) (list prop)))


;;Shorten any non-nil list to a list of one element.

(defun chop (lst)
  (if (consp lst)
      (setf (cdr lst)
            nil))
  x)


;;Destructively tack a symbol onto a list.

(defun ntack (x e)
  (nconc x (list e)))

;;;;;;;;;;;;
;;
;;  Look at the one I came up with.
;;
;; CL-USER> (defun ntack (lst y)
;;            (setf lst ((append lst) (list y))))


;;Draw the cons cell structure that results from the following sequence of operations.

;; (setf x '(a b c))

;; (setf (cdr (last x)) x)


;; S> (setf x '(a b c))
;;                                         ; in: SETF X
;;                                         ;     (SETF X '(A B C))
;;                                         ; ==>
;;                                         ;   (SETQ X '(A B C))
;;                                         ; 
;;                                         ; caught WARNING:
;;                                         ;   undefined variable: COMMON-LISP-USER::X
;;                                         ; 
;;                                         ; compilation unit finished
;;                                         ;   Undefined variable:
;;                                         ;     X
;;                                         ;   caught 1 WARNING condition

;; [*|*]--->[*|*]--->[*|*]--->NIL
;;  |        |        |
;; v        v        v
;; A        B        C

;; Result:  (A B C)

;; S> (setf (cdr (last x)) x)

;; [*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->[*|*]--->etc.
;; |        |        |        |        |        |        |        |
;; v        v        v        v        v        v        v        v
;; A        B        C        A        B        C        A        B



;;The difference between the two

(setf h '(hi ho))

(append h h)

(nconc h h)    ;Obviously don't do this you fuckgenius.



;;10.10 SETQ AND SET

(setf duck 'donald)

(defun test1 (duck)    ;Listing a local duck onto the front of the global duck.
  (list duck
        (symbol-value 'duck)))

(test1 'huey)

(defun test2 (duck)    ;Another local duck.
  (set 'duck 'daffy)    ;Change the global duck.
  (list duck
        (symbol-value 'duck)))

(test2 'huey)

duck

;; CL-USER> (setf duck 'donald)
;; DONALD
;; CL-USER> duck
;; DONALD
;; CL-USER> (symbol-value 'duck)
;; DONALD
;; CL-USER> (list duck)
;; (DONALD)
;; CL-USER> (defun test1 (duck)
;;            (list duck
;;                  (symbol-value 'duck)))
;; TEST1
;; CL-USER> (test1 'huey)
;; (HUEY DONALD)
;; CL-USER> (defun test2 (duck)
;;            (set 'duck 'daffy)
;;            (list duck
;;                  (symbol-value 'duck)))
;; TEST2
;; CL-USER> (test2 'huey)
;; (HUEY DAFFY)
;; CL-USER> duck
;; DAFFY



;;11 Iteration and Block Structure

;;11.2 DOTIMES AND DOLIST

(DOTIMES (index-var n [result-form])
  body)


;;Finding the first odd number in the list iteratively.

(defun find-first-odd (lst)
  (dolist (e lst)
    (format t "~&Testing ~S .." e)
    (when (oddp e)
      (format t ".. found the odd number.")
      (return e))))


;;Checking all numbers on the list for oddity.

(defun check-all-odd (lst)
  (dolist (e lst t)
    (format t "~&Checking ~S for oddity .." e)
    (if (not (oddp e)) (return nil))))

(defun check-all-odd-two (lst)
  (dolist (e lst t)
    (format t "~&Checking ~S for oddity .." e)
    (if (evenp e) (return nil))))


;;Write an iterative MEMBER.

(defun it-member (item lst)
  (dolist (e lst)
    (when (equal item e) (return t))))

(defun it-member-two (obj lst)
  (dolist (e lst)
    (if (equal obj e) (return t))))

(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst)))))

(defun rec-member (obj lst)
  (cond ((null lst) nil)
        ((equal obj (first lst)) lst)
        (t (rec-member obj (rest lst)))))

(defun prim-rec-mem (obj lst)
  (cond ((null lst) nil)
        ((equal obj (car lst)) lst)
        (t (prim-rec-mem obj (cdr lst)))))

;;Write an iterative version of ASSOC.

(defun it-assoc (key table)
  (dolist (entry table)
    (if (equal key (first entry))
        (return entry))))


(defun applic-assoc (key table)
  (find-if #'(lambda (entry)
               (equal key (first entry)))
           table))


(defun cond-assoc (key table)
  (cond ((null table) nil)
        ((equal key (caar table))
         (car table))
        (t (cond-assoc key (cdr table)))))



;;Write a recursive version of CHECK-ALL-ODD

(defun rec-check-all-odd (lst)
  (cond ((null lst) t)
        (t (format t "~&Checking ~S .."
                   (first lst))
           (unless (evenp (first lst))
             (rec-check-all-odd (rest lst))))))



;;11.4 COMPARING RECURSIVE AND ITERATIVE SEARCH

(defun rec-find-first-odd (lst)
  (if (null lst)
      nil
      (if (oddp (first lst))
          (first lst)
          (rec-find-first-odd (cdr lst)))))

(defun cond-find-first-odd (lst)
  (cond ((null lst) nil)
        ((oddp (first lst)) (first lst))
        (t (cond-find-first-odd (rest lst)))))

(defun it-find-first-odd (lst)
  (dolist (e lst)
    (if (oddp e) (return e))))

(defun applica-find-first-odd (lst)
  (find-if #'oddp lst))



;;11.5 BUILDING UP RESULTS WITH ASSIGNMENT

(defun it-fact (n)
  (let ((prod 1))
    (dotimes (i n prod)
      (setf prod (* prod (+ i 1))))))

;;Recursive versions.

(defun cond-fact (n)
  (cond ((zerop n) 1)
        (t (* n (cond-fact (- n 1))))))

(defun if-fact (n)
  (if (zerop n)
      1
      (* n (if-fact (- n 1)))))

(defun it-intersection (set1 set2)
  (let ((result-set nil))
    (dolist (e set1 result-set)
      (when (member e set2)
        (push e result-set)))))


;;Applicative INTERSECTION

(defun appl-intersection (lst1 lst2)
  (remove-if-not
   #'(lambda (e)
       (member e lst2))
   lst1))


;;Write the iterative version of length.

(defun it-length (x)
  (let ((n 0))
    (dolist (e x n)
      (incf n))))


(defun our-length (lst)
  (let ((len 0))    ;From ANSI-Common-Lisp by Paul Graham.
    (dolist (obj lst)
      (setf len (+ 1 len)))
    len))


(defun rec-our-length (lst)
  (if (null lst)    ;Recursive version using IF.
      0
      (+ (rec-our-length (cdr lst)) 1)))


(defun cond-length (lst)
  (cond ((null lst) 0)    ;Recursive version using CONS.
        (t (+ 1 (cond-length (rest lst))))))


;;Write the iterative version of NTH.

(defun it-nth (n lst)
  (dotimes (i n (first lst))
    (pop lst)))


(defun cond-nth (n lst)
  (cond ((null lst) nil)    ;Conditionally recursive.
        ((zerop n) (first lst))
        (t (cond-nth (- n 1) (rest lst)))))


(defun if-nth (n lst)
  (if (null lst)    ;If it can be conditionally recursive ..
      nil
      (if (zerop n)
          (car lst)
          (if-nth (- n 1) (cdr lst)))))



;;Iterative UNION.

(defun it-union (lst1 lst2)
  (dolist (e lst1 lst2)
    (unless (member e lst2)
      (push e lst2))))


(defun it-union-two (lst1 lst2)
  (dolist (e lst1 lst2)
    (unless (member e lst2)
      (setf lst2 (cons e lst2)))))


;;Applicative UNION

(defun appl-union (lst1 lst2)
  (append lst1
          (remove-if
           #'(lambda (e)
               (member e lst1))
           lst2)))

;;Recursive UNION
(defun tr-union (lst1 lst2)
  (cond ((null lst1) lst2)    ;Tail-recursive version.
        ((member (first lst1) lst2)
         (tr-union (rest lst1) lst2))
        (t (tr-union
            (rest lst1)
            (cons (first lst1) lst2)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  11.6 COMPARING DOLIST WITH MAPCAR AND RECURSION
;;
;;

;;An applicative version of show squares.

(defun appl-show-squares (num-lst)
  (mapcar #'(lambda (n) (* n n))
          num-lst))


(defun rec-show-squares (num-list)
  (cond ((null num-list) nil)
        (t (cons (* (first num-list) (first num-list))
                 (rec-show-squares (rest num-list))))))

;;This show-squares is direct from ANSI-Common-Lisp.

(defun acl-show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))


(defun it-show-squares (numlst)
  (let ((result nil))
    (dolist (e numlst (reverse result))
      (push (* e e) result))))    ;Remember this result is being REVERSEd


;;Reverse IT-INTERSECTIONs return value

(defun it-intersection-two (set1 set2)
  (let ((result-set nil))
    (dolist (e set1 (reverse result-set))
      (when (member e set2)
        (push e result-set)))))


;;Write an iterative version of REVERSE.

(defun it-reverse (lst)
  (let ((result nil))
    (dolist (e lst result)
      (push e result))))


;;For the fuck of it.

(defun cond-reverse (lst)
  (cond ((null lst) nil)
        (t (append (reverse (rest lst))
                   (list (first lst))))))


(defun if-reverse (lst)
  (if (null lst)
      nil
      (append (reverse (cdr lst))
              (list (car lst)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  11.7 THE DO MACRO
;;
;;

(do ((var-1 init-1 [update-1])
     (var-2 init-2 [update-2])
     ...)
    (test action-1 ... action-n)
 body)


;;Writing a launching count down.

(defun do-launch (n)
  (do ((cnt n (- cnt 1)))
      ((zerop cnt) (format t "Blast off!"))
    (format t "~S..." cnt)))


;;Write DO-CHECK-ALL-ODD

(defun do-check-all-odd (lst)
  (do ((z lst (rest z)))
      ((null z) t)
    (format t "~&Checking ~S ..." (first z))
    (if (evenp (first z))
        (return nil))))


(defun dolist-check-all-odd (lst)
  (dolist (e lst t)
    (format t "~&Checking ~S ..." e)
    (if (not (oddp e))
        (return nil))))


;;Write DOTIMES-LAUNCH.

(defun dotimes-launch (n)
  (dotimes (i n)
    (format t "~S..." (- n i)))
  (format t "Blast off!"))


;;DO-COUNT-SLICES

(defun do-count-slices (loaf)
  (do ((cnt 0 (+ cnt 1))
       (z loaf (rest z)))
      ((null z) cnt)))


;;Recursive COUNT-SLICES

(defun cond-count-slices (loaf)
  (cond ((null loaf) 0)
        (t (+ 1 (cond-count-slices (rest loaf))))))


(defun if-count-slices (loaf)
  (if (null loaf)
      0
      (+ 1 (if-count-slices (cdr loaf)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  11.8 ADVANTAGES OF IMPLICIT ASSIGNMENT
;;
;;

;;DO-FACT

(defun do-fact (n)
  (do ((i n (- i 1))
       (result 1 (* result i)))
      ((zerop i) result)))


;;COND-FACT

(defun cond-fact (n)
  (cond ((zerop n) 1)
        (t (* n (cond-fact (- n 1))))))


;;IF-FACT

(defun if-fact (n)
  (if (zerop n)
      1
      (* n (if-fact (- n 1)))))


;;IT-INTERSECTION has a null body.

(defun it-intersection (x y)
  (do ((x1 x (rest x1))
       (result nil (if (member (first x1) y)
                       (cons (first x1) result)
                       result)))
      ((null x1) result)))


;;The DO macro form.

(DO ((var-1 init-1 [update-1])
     (var-2 init-2 [update-2])
     ...)
    (test action-1 ... action-n)
 body)


;;Elegantly now ..

(defun it-intersection-elegantly (lst1 lst2)
  (do ((x1 lst1 (rest x1))
       (result nil))
      ((null x1) result)
    (when (member (first x1) lst2)
      (push (first x1) result))))


;;Tail-recursive INTERSECTION using a helper function.

(defun tail-rec-intersection (lst1 lst2)
  (tr-intersect lst1 lst2 nil))

(defun tr-intersect (lst1 lst2 result)
  (cond ((null lst1) result)
        ((member (first lst1) lst2)
         (tr-intersect
          (rest lst1)
          lst2
          (cons (first lst1) result)))
        (t (tr-intersect
            (rest lst1) lst2 result))))


;;Double-test recursive intersection.

(defun cond-intersection (lst1 lst2)
  (cond ((null lst1) nil)
        ((member (first lst1) lst2)
         (cons (first lst1)
               (cond-intersection (rest lst1) lst2)))
        (t (cond-intersection (rest lst1) lst2))))


;;Elegantly again ..

(defun it-intersection-elegantly (lst1 lst2)
  (do ((x1 lst1 (rest x1))
       (result nil))
      ((null x1) result)
    (when (member (first x1) lst2)
      (push (first x1) result))))


;;Comparing corresponding elements from two lists until two equal ones are found.

(defun it-find-matching-elements (lst1 lst2)
  (do ((x lst1 (rest x))
       (y lst2 (rest y)))
      ((or (null x) (null y) nil))
    (if (equal (first x)
               (first y))
        (return (first x)))))


;;Find the first odd number with DO.

(defun do-find-first-odd (lst)
  (do ((x lst (rest x)))
      ((null x) nil)
    (if (oddp (first x)) (return (first x)))))


;;Now with DO*.

(defun do*-find-first-odd (lst)
  (do* ((x lst (rest x))
        (e (first x) (first x)))
       ((null x) nil)
    (if (oddp e) (return e))))


;;Rewrite for DO* instead.

(defun dolist-find-largest (lst)
  (let ((largest (first lst)))
    (dolist (e (rest lst)
               largest)
      (when (> e largest)
        (setf largest e)))))


(defun do*-find-largest (lst)
  (do* ((largest (car lst))
        (z (cdr lst) (cdr z))
        (e (car z) (car z)))
       ((null z) largest)
    (when (> e largest)
      (setf largest e))))


(defun if-do*-find-largest (lst)
  (do* ((largest (first lst))
        (z (rest lst) (rest z))
        (element (first z) (first z)))
       ((null z) largest)
    (if (> element largest)
        (setf largest element))))


;;With DO instead of DOTIMES.

(defun dotimes-power-of-2 (n)    ;2 to the Nth power.
  (let ((result 1))
    (dotimes (i n result)
      (incf result result))))


(defun do-power-of-two (n)    ;Two to the Nth power.
  (do ((result 1 (+ result result))
       (i 0 (+ i 1)))
      ((equal i n) result)))



;;Write with DOLIST instead of DO*.

(defun do*-first-non-integer (lst)
  (do* ((z lst (rest z))
        (element (first z) (first z)))
       ((null z) 'none)
    (unless (integerp element)
      (return element))))


(defun dolist-first-non-integer (lst)
  (dolist (element lst 'none)
    (unless (integerp element)
      (return element))))


(defun dolist-when-first-int (lst)
  (dolist (element lst 'none)
    (when (not (integerp element))
      (return element))))



;;Swap DO* with DO, what bug does this introduce?

(defun do*-find-first-odd (lst)
  (do* ((x lst (rest x))
        (e (first x) (first x)))
       ((null x) nil)
    (if (oddp e) (return e))))


(defun buggy-do-find-first-odd (lst)
  (do ((x lst (rest x))
        (e (first x) (first x)))
       ((null x) nil)
    (if (oddp e) (return e))))



;;Find the bug.

(defun subtle-buggy-do-ffo (lst)
  (do ((z lst (rest z))
       (element (first lst) (first z)))
      ((null z) nil)
    (if (oddp element) (return element))))

(defun fixed-subtle-buggy-do-ffo (lst)
  (do ((z lst (rest z))
       (element (first lst) (first z)))
      ((null z) element)
    (if (oddp element) (return element))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  11.10 INFINITE LOOPS WITH DO
;;
;;

;;DO me infinitely baby.

(defun do-read-a-num ()
  (do ((answer nil))
      (nil)
    (format t "~&Type a number: ")
    (setf answer (read))
    (if (numberp answer)
        (return answer))
    (format t
            "~&Bitch, ~S isn't a number. Give me a number DAMNIT."
            answer)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  11.11 IMPLICIT BLOCKS
;;
;;

(DOLIST (index-var list [result-form])
  body)


(DOTIMES (index-var list [result-form])
  body)


(defun block-dolist-ffo (lst)
  (format t "~&Looking for the odd number ..")
  (dolist (element lst)
    (when (oddp element)
      (format t "~&Found ~S." element)
      (return-from block-dolist-ffo element)))
  (format t "~&None found.")
  'none)


(defun mapcar-sq-list (lst)
  (mapcar
   #'(lambda (e)
       (if (numberp e)
           (* e e)
           (return-from mapcar-sq-list 'nope)))
   lst))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CHAPTER 11 REVIEW EXERCISES
;;
;;

(DOLIST (index-var list [result-form])
  body)

;;How LET and DO forms differ.

(LET ((var-1 value-1)
      (var-2 value-2)
      ...
      (var-n value-n))
  body)


(DO ((var-1 init-1 [update-1])
     (var-2 init-2 [update-2])
     ...)
    (test action-1 ... action-n)
 body)


;;What is the value returned by the following expression?

(dotimes (i 5 i)
  (format t "~&I = ~S" i))


;;Rewrite that with DO.

(do ((i 0 (+ i 1)))
    ((equal i 5) i)
  (format t "~%I = ~S" i))


;;Does switching variable entries in the DO form fuck something up?

(defun do-count-slices (loaf)
  (do ((cnt 0 (+ cnt 1))
       (z loaf (rest z)))
      ((null z) cnt)))

(defun do-switch-count-slices (loaf)
  (do ((z loaf (rest z))
       (cnt 0 (+ cnt 1)))
      ((null z) cnt)))


;;Write the iterative versions of FIB.

(defun do*-fib (n)    ;DO* version.
  (do* ((cnt 0 (+ cnt 1))
        (i 1 j)
        (j 1 k)
        (k 2 (+ i j)))
       ((equal cnt n) i)))


(defun do-fib (n)
  (do ((cnt 0 (+ cnt 1))
       (i 1 j)
       (j 1 (+ i j)))
      ((equal cnt n) i)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  CH11 KEYBOARD EXERCISE
;;
;;

;;Examining DNA and RNA with Lisp.

;; (a g g t c a t t g)  <=> (t c c a g t a a c)


;;Translates the base to their corresponding base.

(defun complement-base (base)
  (second (assoc base '((a t) (t a) (g c) (c g)))))


;;Matching strands of bases.

(defun do-complement-strand (strand)
  (do ((b strand (cdr b))
       (result nil
               (cons (complement-base (car b))
                     result)))
      ((null b) (reverse result)))) 


(do ((var-1 init-1 [update-1])
     (var-2 init-2 [update-2])
     ...)
    (test action-1 ... action-n)
 body)


(dolist (index-var list [result-form])
  body)


;;Gives a double-stranded version of strand.

(defun do-make-double (strand)
  (do ((b strand (rest b))
       (result nil
               (cons (list (first b) (complement-base (first b)))
                     result)))
      ((null b) (reverse result))))


;;Count the number of bases of each type in a DNA strand, returning a table.
;;Note that the LABELS is defining a local function that acts as a counter.
;;The LET form establishes our counting variables.

(defun count-bases (dna)
  (let ((acnt 0) (tcnt 0) (gcnt 0) (ccnt 0))
    (labels ((cond-count-one-base (base)
               (cond ((equal base 'a) (incf acnt))
                     ((equal base 't) (incf tcnt))
                     ((equal base 'g) (incf gcnt))
                     ((equal base 'c) (incf ccnt)))))
      (dolist (element dna)
        (cond ((atom element) (cond-count-one-base element))
              (t (cond-count-one-base (first element))
                 (cond-count-one-base (second element)))))
      (list (list 'a acnt)
             (list 't tcnt)
             (list 'g gcnt)
             (list 'c ccnt)))))


;;Is one strand of DNA the prefix of another ..

(defun prefixp (strand1 strand2)
  (do ((s1 strand1 (rest s1))
       (s2 strand2 (rest s2)))
      ((null s1) t)
    (unless (equal (first s1) (first s2))
      (return nil))))


;;T if one dna strand appears anywhere within another dna strand.

(defun appearsp (strand-1 strand-2)
  (do ((s2 strand-2 (rest s2)))
      ((null s2) nil)
    (if (prefixp strand-1 s2)
        (return t))))


;;Checks if a strand is repeated within the other, NIL for anything other.

(defun coverp (strand-1 strand-2)
  (do* ((len1 (length strand-1))
        (s2 strand-2 (nthcdr len1 s2)))
       ((null s2) t)
    (unless (prefixp strand-1 s2)
      (return nil))))


;;Return the leftmost N bases of a DNA strand.

(defun prefix (n strand)
  (do ((i 0 (+ i 1))
       (result nil (cons (nth i strand) result)))
      ((equal i n) (reverse result))))


;;Return the shortest prefix of a dna strand that can be repeated to cover the input strand.

(defun kernel (strand)
  (do ((i 1 (+ i 1)))
      ((coverp (prefix i strand) strand)
       (prefix i strand))))


;;Takes a single-stranded dna sequence as input
;;and draws it along with the complementary strand.

(defun draw-dna (strand)
  (let ((n (length strand)))
    (draw-string n "-----")
    (draw-string n "  !  ")
    (draw-bases strand)
    (draw-string n "  .  ")
    (draw-string n "  .  ")
    (draw-bases (complement-strand strand))
    (draw-string n "  !  ")
    (draw-string n "-----")))


(defun draw-string (cnt string)
  (format t "~&")
  (dotimes (i cnt)
    (format t "~A" string)))


(defun draw-bases (strand)
  (format t "~&")
  (dolist (base strand)
    (format t "  ~A  " base)))


(LET ((var-1 val-1)
      (var-2 val-2)
      ...
      (var-n val-n))
  body)

(DOTIMES (index-var list [result-form])
  body)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Lisp Toolkit: TIME
;;
;;

(defun addup (n)
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum i)))
      ((> i n) sum)))



;;Let's review iteration.

(let ((result 0))
  (dolist (elem '(4 5 6 7) result)    ;With DOLIST.
    (setq result (+ result elem))))

(defun add-up-numbers (numbers)
  (let ((result 0))
    (dolist (elem numbers result)
      (setq result (+ result elem)))))


(let ((result 1))
  (dotimes (n 10 result)
    (setq result (+ result n))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  11 Advanced Topics
;;
;;

;;PROG1 PROG2 PROGN

(prog1 (setf x 'foo) (setf x 'bar) (setf x 'baz)
       (format t "~&X is ~S" x))


(prog2 (setf x 'foo) (setf x 'bar) (setf x 'baz)
       (format t  "~&X is ~S" x))


(progn (setf x 'foo) (setf x 'bar) (setf x 'baz)
       (format t "~&X is ~S" x))


;;Equivalent to (POP X).

(defun prog-pop (lst)
  (prog1 (first lst) (setf lst (rest lst))))


(defun let-pop (lst)
  (let ((was-on-top (first lst)))
    (setf lst (rest lst))
    was-on-top))


;;11.13 OPTIONAL ARGUMENTS

(defun optional-foo (x &optional y)
  (format t "~&X is ~S" x)
  (format t "~&Y is ~S" y)
  (list x y))


(defun divide-check (dividend &optional (divisor 2))
  (format t "~&~S ~A divide evenly by ~S"    ;Default value set to 2.
          dividend
          (if (zerop (rem dividend divisor))
              "does"
              "does not")
          divisor))


;;11.14 REST ARGUMENTS

(defun rest-args-avg (&rest args)
  (/ (reduce #'+ args)
     (length args)
     1.0))


(defun faulty-square-all (&rest nums)
  (if (null nums)
      nil
      (cons (* (first nums) (first nums))
            (faulty-square-all (rest nums)))))


(defun square-the-rest (&rest args)
  (if (null args)
      nil
      (cons (* (first args) (first args))
            (apply #'square-the-rest (rest args)))))


;;Defining PROG1 PROG2 and PROGN with the &REST lambda-list-keyword.

;; CL-USER> (defun my-prog1 (x &rest ignore) x)
;; MY-PROG1

;; CL-USER> (my-prog1 (setf x 'foo)
;;                    (setf x 'bar)
;;                    (setf x 'baz)
;;                    (format t "~&X is ~S" x))
;; X is BAZ
;; FOO


;; CL-USER> (defun my-prog2 (x y &rest ignore) y)
;; MY-PROG2

;; CL-USER> (my-prog2 (setf x 'foo)
;;                    (setf x 'bar)
;;                    (setf x 'baz)
;;                    (format t "~&X is ~S" x))
;; X is BAZ
;; BAR


;; CL-USER> (defun my-progn (&rest x)
;;            (car (last x)))
;; MY-PROGN

;; CL-USER> (my-progn (setf x 'foo)
;;                    (setf x 'bar)
;;                    (setf x 'baz)
;;                    (format t "~&X is ~S" x))
;; X is BAZ
;; NIL



;;11.15 KEYWORD ARGUMENTS

(member x y :test #'equal)


;;MAKE-SUNDAE takes up to five keyword arguments.

(defun make-sundae (name &key (size 'regular) (ice-cream 'vanilla) (syrup 'hot-fudge)
                              nuts
                              cherries
                              whipped-cream)
  (list 'sundae
        (list 'for name)
        (list ice-cream 'with syrup 'syrup)
        (list 'toppings '=
              (remove nil
                      (list (and nuts 'nuts)
                            (and cherries 'cherries)
                            (and whipped-cream 'whipped-cream))))))


;;11.16 AUXILIARY VARIABLES

;;Recall the average function with the &REST arg.

(defun rest-args-avg (&rest args)
  (/ (reduce #'+ args)
     (length args)
     1.0))

;;Look at this one.

(defun average (&rest args &aux (len (length args)))
  (/ (reduce #'+ args) len 1.0))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  12 Structures and The Type System
;;
;;


;;12.3 DEFINING STRUCTURES

(defstruct starship
  (name nil)
  (speed 0)
  (condition 'green)
  (shields 'down))

(setf s2 '#s(starship speed (warp 3)
                      condition rede
                      shields up))


;;12.5 ACCESSING AND MODIFYING STRUCTURES


;;Raising the alert.

(defun alert (ship-name)
  (setf (starship-shields ship-name) 'up)
  (if (equal (starship-condition ship-name) 'green)
      (setf (starship-condition ship-name) 'yellow))
  'shields-raised)


;;12.6 KEYWORD ARGUMENTS TO CONSTRUCTOR FUNCTIONS

;;Making a new instance, setting our own values for it.

(setf s3 (make-starship :name "Bung-Traveler"
                        :shields 'damaged))


;;12.7 CHANGING STRUCTURE DEFINITIONS

;;Adding things to the STARSHIP structure.

(defstruct starship
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed o))


;;Now we rebuild our old structures.

(setf s1 (make-starship))

(setf s2 (make-starship :speed (warp 3)
                        :condition 'red
                        :shields 'up))

(setf s3 (make-starship :captain "Benson"
                        :name "Reliant"
                        :shields 'damaged))


;;Raise the alert again on S!.

(defun raise-alert (ship)
  (setf (starship-shields ship) 'up)
  (if (equal (starship-condition ship) 'green)
      (setf (starship-condition ship) 'yellow))
  'shields-raised)

(raise-alert s1)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    Chapter 12 Keyboard Exercise
;;
;;


;;Create the NODE structure with the four components.

(defstruct node
  name
  question
  yes-case
  no-case)


;;Define the global variable *NODE-LIST* and write the initialization function.

(setf *node-list* nil)


(defun network-initialization ()
  (setf *node-list* nil)
  'initialized)


;;We need to add nodes.

(defun add-node (name question
                 yes-case no-case)
  (push (make-node :name name
                   :question question
                   :yes-case yes-case
                   :no-case no-case)
        *node-list*)
  name)


;;We need to find nodes.

(defun find-node (input)
  (find-if #'(lambda (node)
               (equal (node-name node) input))
           *node-list*))


;;Process nodes.

(defun process-node (name)
  (let ((nd (find-node name)))
    (if (y-or-n-p "~&~A " (node-question nd))
        (node-yes-case nd)
        (node-no-case nd))
    (format t "~&Node ~S not yet defined." name)))

;;Updating a current node with a processed one.

(defun run ()
  (do ((current-node 'start (process-node current-node)))
      ((null current-node) nil)
    (cond ((stringp current-node)
           (format t "~&~A" current-node)
           (return nil)))))


;;Interactively adding new nodes to the net.

(defun interactive-add ()
  (let* ((name (format t "Node name? "))
         (question (format t "Question? "))
         (yes-action (format t "If yes? "))
         (no-action (format t "If no? ")))
    (add-node name question yes-action no-action)))


;;Code for the automotive diagnosis network.

(add-node 'start
          "Does the engine turn over?"
          'engine-turns-over
          'engine-wont-turn-over)

(add-node 'engine-turns-over
          "Will the engine run for any period of time?"
          'engine-will-run-briefly
          'engine-wont-run)

(add-node 'engine-wont-run
          "Is there gas in the tank?"
          'gas-in-tank
          "Fill the tank and try starting the engine again.")

(add-node 'engine-wont-turn-over
          "Do you hear any sound when you turn the keys?"
          'sound-when-turn-key
          'no-sound-when-turn-key)

(add-node 'no-sound-when-turn-key
          "Is the battery voltage low?"
          "Replace the battery"
          'battery-voltage-ok)

(add-node 'batter-voltage-ok
          "Are the batter cables dirty or loose?"
          "Clean the battery cables and tighten the connections."
          'battery-cables-good)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;    12 Advanced Topics
;;
;;


;;12.8 Printing specified components for structures.

(defun print-starship (x stream depth)
  (format stream "#<STARSHIP ~A>"
          (starship-name x)))


;;Defining our structure so that it prints our components implicitly.

(defstruct (starship
            (:print-function print-starship))
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed 0))



;;Chapter 12 Advanced Exercise

(setf s1 (make-starship :name "Enterprise"))

(defstruct (captain
            (:print-function print-captain))
  (name nil)
  (age nil)
  (ship nil))

(defun print-captain (x stream depth)
  (format stream "#<CAPTAIN ~S>"
          (captain-name x)))

(setf jim (make-captain
           :name "James T. Kirk"
           :age 35
           :ship s1))

(setf (starship-captain s1) jim)


;;12.10 Structure Inheritance

(defstruct ship
  (name nil)
  (captain nil)
  (crew-size nil))

(defstruct (starship
            (:include ship)
            (:print-function print-starship))
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed 0)
  (weapons nil))

(defstruct (supply-ship
            (:include ship)
            (:print-function print-supply-ship))
  (cargo nil))

(defun print-supply-ship (ship stream depth)
  (format stream "#<SUPPLY-SHIP ~A>"
          (supply-ship-name ship)))


(setf *alpaca-one* (make-supply-ship
                    :captain "Harry Dingle"
                    :crew-size 43
                    :cargo 'fuel))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  13 Arrays, Hash Tables, and Property Lists
;;
;;


;;13.2 CREATING AN ARRAY

