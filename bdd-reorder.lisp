(in-package #:bdd)

;; aborted attempt at implementing variable reordering
;; the bdd struct needs to be altered to have an array
;; of hash tables, one per variable index/level.
;;
;; swap and shift seem to work, but we need a good way
;; to know what shifts shrink the table, and it seems
;; incorrect to utilize the fluctuating size of the weak-ht
;; unique table.

(defconstant reorder-threshold-factor 1.2
  "Threshold factor for determining when to sift variables.")

(defconstant reorder-start-threshold 100
  "Size of the DAG before starting to sift variables.")

(defun variable-level (v)
  "Get level of variable V in *BDD*'s BDD base. "
  (aref (bdd-base *bdd*) (variable-index v)))

(defun variable-at-index (i)
  "Lookup the variable currently at index I."
  (svref (bdd-index-variable *bdd*) i))

(defun bdd-base-size (bdd-base)
  "Compute the total number of nodes in BDD-BASE."
  (declare (type bdd-base bdd-base))
  (the fixnum
       (reduce (lambda (size h) (+ size (hash-table-count h)))
               bdd-base
               :initial-value 0)))

(defun bdd-size (bdd)
  "Number of nodes currently allocated in BDD."
  (declare (type bdd bdd))
  (bdd-base-size (bdd-base bdd)))

(defconstant reorder-growth-factor 2
  "Factor for determining how large the DAG grows before reordering.")

(defun needs-reordering-p (bdd)
  "Does BDD need variable reordering?"
  (> (bdd-base-size (bdd-base bdd))
     (* reorder-threshold-factor (bdd-last-size bdd))))

(defun swap (x y)
  "Swap the variable X with it's successor Y."
  (flet ((swap-node (v f)
           (declare (ignore v))
           (let* ((f0 (node-lo f))
                  (f1 (node-hi f))
                  (f00 (cofactor f0 y nil))
                  (f01 (cofactor f0 y t))
                  (f10 (cofactor f1 y nil))
                  (f11 (cofactor f1 y t)))
             (setf (node-var f) y
                   (node-lo f) (find-or-add x f00 f10)
                   (node-hi f) (find-or-add x f01 f11)))))
    (let ((level (variable-level x)))
      (rotatef (variable-index x) (variable-index y))
      (maphash #'swap-node level))))

(defun shift (x target)
  "Shift a variable X to TARGET level by repeated swaps."
  (do* ((j (variable-index x)
           (variable-index x))
        (y (variable-at-index (1+ j))
           (variable-at-index (1+ j))))
       ((= target j))
    (swap x y)))

(defun sift (v)
  "Shift variable V to its optimum position in *BDD*."
  (declare (type var v)
           #+nil(optimize (speed 3))
           (ignore v)))

(defvar *reordering-p* nil
  "Are we currently reordering variables?")

(defun reorder ()
  "Reorder variables in *BDD*, attempting to shrink the DAG."
  (let ((*reordering-p* t))
    (setf (bdd-last-size *bdd*)
          (bdd-base-size (bdd-base *bdd*)))))

(defun maybe-reorder ()
  "Reorder if necessary and not already reordering."
  (when (and (not *reordering-p*) (needs-reordering-p *bdd*))
    (reorder)))

(defun reorder-after (name)
  "Instrument function named NAME with advice to reorder."
  (setf (fdefinition name)
        (let ((old (fdefinition name)))
          (lambda (&rest args)
            (prog1 (cl:apply old args)
              (maybe-reorder))))))

(defun reorder-before (name)
  "Instrument function named NAME with advice to reorder."
  (setf (fdefinition name)
        (let ((old (fdefinition name)))
          (lambda (&rest args)
            (progn (maybe-reorder)
                   (cl:apply old args))))))
