;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun analyze-profit (price commission-rate)
  (let* ((commission (* price commission-rate))
         (result
           (cond ((> commission 100) 'rich)
                 (t (< commission 100) 'poor))))
    (break "Value of RESULT is ~S" result)
    (format t "~&I predict you will be: ~S"
            result)
    result))

