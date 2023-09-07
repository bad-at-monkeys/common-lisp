;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Hash Table Keyboard Exercise
;;
;;

;;The cryptogram to be solved.

;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; enlpo pib slafml pvv bfwkj

;;We need to store this cryptogram as a list of strings in some variable.

(setf cryptogram-text '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
                        "enlpo pib slafml pvv bfwk"))

;; ("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
;;  "enlpo pib slafml pvv bfwk")


;;Store our cryptogram data into two hash tables.

(setf *decipher-table* (make-hash-table))

(setf *encipher-table* (make-hash-table))


;;This function should take two character objects and store them in the hash tables.

(defun make-substitution (code clear)
  (setf (gethash clear *encipher-table*) code)
  (setf (gethash code *decipher-table*) clear))


;;Now we need a function that undoes any substitution mistakes.

(defun undo-substitution (code clear)
  (setf (gethash clear *encipher-table*) nil)
  (setf (gethash clear *decipher-table*) nil))


;;Write a function that clears the two hash tables at once.

(defun clear-hash-tables ()
  (clrhash *encipher-table*)
  (clrhash *decipher-table*))


;;Write a way to partially decipher an input string.

(defun decipher-string (string)
  (do* ((length-of-string (length string))
        (new-string (make-string length-of-string
                                 :initial-element #\Space))
        (i 0 (1+ i)))
       ((equal i length-of-string) new-string)
    (let* ((character (aref string i))
           (new-character (gethash character *decipher-table*)))
      (when new-character
        (setf (aref new-string i) new-character)))))


;;Create a way to display cryptogram text with a line of deciphered characters beneath.

(defun show-line (line)
  (format t "~%~A~%~A~%"
          line
          (decipher-string line)))


(defun show-text ()
  (format t "~&----------------")
  (dolist (line cryptogram-text)
    (show-line line))
  (format t "~&----------------"))


;;Return the first character in the lowercase printed representation of an object.

(defun get-first-char (x)
  (char-downcase
   (char (format nil "~A" x) 0)))


;;Read an object from the keyboard and return that value if it is END or UNDO.
;;Otherwise, GET-FIRST-CHAR should be used on the object to extract and return that character.

(defun read-letter ()
  (let ((obj (read)))
    (if (member obj '(end undo))
        obj
        (get-first-char obj))))


;;Ask the user what the letter deciphers to.

(defun sub-letter (code)
  (when (gethash code *decipher-table*)
    (format t "~&'~A' has already been" code)
    (format t "deciphered as '~A'!"
            (gethash code *decipher-table*))
    (return-from sub-letter nil))
  (format t "What does '~A' decipher to? " code)
  (let ((clear (read-letter)))
    (cond ((not (characterp clear))
           (format t "~&Invalid response."))
          ((gethash clear *encipher-table*)
           (format t "But '~A' already"
                   (gethash clear *encipher-table*))
           (format t " deciphers as '~A'!"
                   clear))
          (t (make-substitution code clear)))))


;;Write the main function SOLVE.

(defun solve (string-1 string-2)
  (do ((resp nil))
      ((equal resp 'end))
    (show-text)
    (format t "~&Substitute which letter? ")
    (setf resp (read-letter))
    (cond ((characterp resp) (sub-letter resp))
          ((equal resp 'undo) (undo-letter))
          ((equal resp 'end) nil)
          (t (format t "~&Invalid input.")))))



;; CL-USER> (solve '(zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf) '(enlpo pib slafml pvv bfwk))
;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf


;; enlpo pib slafml pvv bfwk

;; ----------------
;; Substitute which letter? z
;; What does 'z' decipher to? i

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; i  i                i   i                                  

;; enlpo pib slafml pvv bfwk

;; ----------------
;; Substitute which letter? j
;; What does 'j' decipher to? t

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it i    tt   t      i   i   t        t     t        t    t 

;; enlpo pib slafml pvv bfwk

;; ----------------
;; Substitute which letter? e
;; What does 'e' decipher to? s

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is   tt   t      i  si   t        t     t        t    t 

;; enlpo pib slafml pvv bfwk
;; s                        
;; ----------------
;; Substitute which letter? k
;; What does 'k' decipher to? b

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is b tt   t      i  si   t     b  t     t        t    t 

;; enlpo pib slafml pvv bfwk
;; s                       b
;; ----------------
;; Substitute which letter? l
;; What does 'l' decipher to? e

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is bette  t   e  i  si e t     be t     t        t    t 

;; enlpo pib slafml pvv bfwk
;; s e        e   e        b
;; ----------------
;; Substitute which letter? s
;; What does 's' decipher to? r

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better t  re  i  si e t     be t     t        t    t 

;; enlpo pib slafml pvv bfwk
;; s e       re   e        b
;; ----------------
;; Substitute which letter? f
;; What does 'f' decipher to? o

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to re  i  si e t     be t o   t    oo  t    to

;; enlpo pib slafml pvv bfwk
;; s e       re o e      o b
;; ----------------
;; Substitute which letter? a
;; What does 'a' decipher to? m

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to rem i  si e t     be t o   t    oo  t    to

;; enlpo pib slafml pvv bfwk
;; s e       remo e      o b
;; ----------------
;; Substitute which letter? p
;; What does 'p' decipher to? a

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remai  si e t a   be t o   t a  oo  t a  to

;; enlpo pib slafml pvv bfwk
;; s ea  a   remo e a    o b
;; ----------------
;; Substitute which letter? i
;; What does 'i' decipher to? n

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain si ent an  be t o   t a  oo  t an to

;; enlpo pib slafml pvv bfwk
;; s ea  an  remo e a    o b
;; ----------------
;; Substitute which letter? v
;; What does 'v' decipher to? l

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent an  be t o   t a  ool t an to

;; enlpo pib slafml pvv bfwk
;; s ea  an  remo e all  o b
;; ----------------
;; Substitute which letter? b
;; What does 'b' decipher to? d

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be t o   t a  ool t an to

;; enlpo pib slafml pvv bfwk
;; s ea  and remo e all do b
;; ----------------
;; Substitute which letter? u
;; What does 'u' decipher to? h

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be tho  ht a  ool than to

;; enlpo pib slafml pvv bfwk
;; s ea  and remo e all do b
;; ----------------
;; Substitute which letter? w
;; What does 'w' decipher to? u

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be thou ht a  ool than to

;; enlpo pib slafml pvv bfwk
;; s ea  and remo e all doub
;; ----------------
;; Substitute which letter? x
;; What does 'x' decipher to? g

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be thought a  ool than to

;; enlpo pib slafml pvv bfwk
;; s ea  and remo e all doub
;; ----------------
;; Substitute which letter? h
;; What does 'h' decipher to? f

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be thought a fool than to

;; enlpo pib slafml pvv bfwk
;; s ea  and remo e all doub
;; ----------------
;; Substitute which letter? n
;; What does 'n' decipher to? p

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be thought a fool than to

;; enlpo pib slafml pvv bfwk
;; spea  and remo e all doub
;; ----------------
;; Substitute which letter? o
;; What does 'o' decipher to? k

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be thought a fool than to

;; enlpo pib slafml pvv bfwk
;; speak and remo e all doub
;; ----------------
;; Substitute which letter? m
;; What does 'm' decipher to? v

;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be thought a fool than to

;; enlpo pib slafml pvv bfwk
;; speak and remove all doub
;; ----------------
;; Substitute which letter? nil

;; 'n' has already beendeciphered as 'p'!
;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be thought a fool than to

;; enlpo pib slafml pvv bfwk
;; speak and remove all doub
;; ----------------
;; Substitute which letter? 


;; q
;; What does 'q' decipher to? exit
;; But 'l' already deciphers as 'e'!
;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be thought a fool than to

;; enlpo pib slafml pvv bfwk
;; speak and remove all doub
;; ----------------
;; Substitute which letter? 0
;; What does '0' decipher to? ()
;; But 'i' already deciphers as 'n'!
;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be thought a fool than to

;; enlpo pib slafml pvv bfwk
;; speak and remove all doub
;; ----------------
;; Substitute which letter? poop

;; 'p' has already beendeciphered as 'a'!
;; ----------------
;; zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf
;; it is better to remain silent and be thought a fool than to

;; enlpo pib slafml pvv bfwk
;; speak and remove all doub
;; ----------------
;; Substitute which letter? ; Evaluation aborted on NIL.
