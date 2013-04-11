(setf *random-state* (make-random-state t))

(defvar factor 0.04)
(defvar litter-size 100)
(defvar uber-weasel "METHINKS IT IS LIKE A WEASEL")
(defvar gene-pool "ABCDEFGHIJKLMNOPQRSTUVWXYZ ")

(defun random-gene ()
  (char gene-pool (random (length gene-pool))))

(defun random-genes (len)
  (loop for i to (- len 1) collect (random-gene)))

(defclass weasel ()
  ((genes :accessor genes
          :initform (coerce (random-genes (length uber-weasel)) 'string)
          :initarg :genes)))

(defun mutate-genes (genes)
  (coerce (mapcar (lambda (c)
             (if (< (random 1.0) factor)
                 (random-gene)
                 c)) (copy-seq (coerce genes 'list))) 'string))

(defmethod create-litter ((w weasel))
  (loop for i to litter-size
     collect (make-instance 'weasel :genes (mutate-genes (genes w)))))

(defmethod compare-to-goal ((w weasel))
  (let ((a (coerce (genes w) 'list))
        (b (coerce uber-weasel 'list)))
    (reduce #'+ (mapcar #'(lambda (q w)
                 (if (char= q w) 1 0)) a b))))

(defun weasel< (a b)
  (> (compare-to-goal a) (compare-to-goal b)))

(defun get-best (litter)
  (car;nth litter-size
   (sort
    (copy-seq litter) #'weasel<)))

(defun evolution (weasel count)
  (format t "~D -> ~a~%"
          count (genes weasel))
  (if (string=
       (genes weasel) uber-weasel)
      '(count uber-weasel)
      (evolution
       (get-best (create-litter weasel)) (+ 1 count))))

(evolution (make-instance 'weasel) 0)


