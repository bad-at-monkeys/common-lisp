;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  A histogram that records how many times RANDOM
;;    has chosen the same instance of a number.
;;


;;Create a variable to hold the array of instances.

(setf *histogram-array* nil)


;;Store the total number of instances recorded.

(setf *total-instances* 0)


;;We need an initialization function specifying the number of buckets.

(defun new-histogram (bins)
  (setf *total-instances* 0)
  (setf *histogram-array*
        (make-array bins :initial-element 0))
  t)


;;We need a method for recording instances of the same number.

(defun record-value (value)
  (incf *total-instances*)
  (if (and (>= value 0)
           (< value (length *histogram-array*)))
      (incf (aref *histogram-array* value))
      (error "Value ~S out of bounds." value)))


;;Print the array bin's line.

(defun print-histogram-line (bin)
  (let ((value (aref *histogram-array* bin)))
    (format t "~&~2D [~3D] " bin value)
    (dotimes (i value)
      (format t "*"))))


;;Finally print the histogram

(defun print-histogram ()
  (dotimes (i (length *histogram-array*))
    (print-histogram-line i))
  (format t "~&    ~3D total" *total-points*))


