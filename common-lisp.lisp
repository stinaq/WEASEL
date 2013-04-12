(setf *random-state* (make-random-state t))

(defvar factor 0.04)
(defvar litter-size 100)
(defvar uber-weasel (coerce "METHINKS IT IS LIKE A WEASEL" 'list))
(defvar gene-pool "ABCDEFGHIJKLMNOPQRSTUVWXYZ ")

(defun random-gene ()
  (char gene-pool (random (length gene-pool))))

(defun random-genes (len)
  (loop for i to (- len 1) collect (random-gene)))

(defclass weasel ()
  ((genes :accessor genes
          :initform (random-genes (length uber-weasel))
          :initarg :genes)))

(defun mutate-genes (genes)
  (mapcar (lambda (c)
            (if (< (random 1.0) factor)
                (random-gene)
                c))
          genes))

(defmethod create-litter ((w weasel))
  (loop for i to litter-size
     collect (make-instance 'weasel :genes (mutate-genes (genes w)))))

(defmethod compare-to-goal ((w weasel))
  (reduce #'+ (mapcar #'(lambda (q w)
                          (if (char= q w) 1 0))
                      (genes w) uber-weasel)))

(defun weasel> (a b) ;descending
  (> (compare-to-goal a) (compare-to-goal b)))

(defun get-best (litter)
  (car
   (sort
    (copy-seq litter) #'weasel>)))

(defun evolution (weasel count)
  (let ((gene-string (coerce (genes weasel) 'string)))
    (format t "~D -> ~a~%"
            count gene-string)
    (if (equal
         (genes weasel) uber-weasel)
        (list count (coerce uber-weasel 'string))
        (evolution
         (get-best (create-litter weasel)) (+ 1 count)))))

(print (evolution (make-instance 'weasel) 0))


