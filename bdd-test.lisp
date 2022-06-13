(defpackage #:bdd-test
  (:use #:cl #:fiveam)
  (:import-from
   #:bdd
   #:make-bdd
   #:*bdd*
   #:unit
   #:ite
   #:xor
   #:nor
   #:nand
   #:true
   #:false
   #:conjunct
   #:disjunct
   #:negate
   #:any-sat
   #:all-sat
   #:sat-count)
  (:export
   #:n-queens
   #:full-adder
   #:circuit-equivlance))

(in-package #:bdd-test)

(defun n-queens-formula (n)
  (let* ((vars (loop :for i :from 1 :to n
                     :collect
                     (loop :for j :from 1 :to n
                           :collect
                           (intern (format nil "X_~D_~D" i j)))))
         (*bdd* (make-bdd (cl:apply #'append vars)))
         (vars (make-array (list n n) :initial-contents vars)))
    (labels ((x (i j) (aref vars (1- i) (1- j)))
             (mutex (v)
               (apply #'conjunct
                      (loop :for x :in v
                            :collect
                            (ite x
                                 (negate
                                  (apply #'disjunct
                                         (loop :for y :in v
                                               :when (not (eql y x))
                                                 :collect y)))
                                 true))))
             (at-least-one-queen-per-row (n)
               (apply #'conjunct
                      (loop :for i :from 1 :to n
                            :collect
                            (apply #'disjunct
                                   (loop :for j :from 1 :to n
                                         :collect (unit (x i j)))))))
             (at-most-one-queen-per-line (row n)
               (apply #'conjunct
                      (loop :for i :from 1 :to n
                            :for vars := (loop :for j :from 1 :to n
                                               :collect (if row
                                                            (unit (x i j))
                                                            (unit (x j i))))
                            :collect (mutex vars))))
             (at-most-one-queen-per-diagonal (slash n)
               (multiple-value-bind (a b)
                   (if slash
                       (values (- n) n)
                       (values 0 (* 2 n)))
                 (apply #'conjunct
                        (loop :for k :from a :to b
                              :for ij := (loop :for i :from 1 :to n
                                               :collect (if slash
                                                            (cons i (+ i k))
                                                            (cons i (- k i))))
                              :for xijs := (loop :for (i . j) :in ij
                                                 :when (and (<= 1 i n)
                                                            (<= 1 j n))
                                                   :collect (unit (x i j)))
                              :when xijs
                                :collect (mutex xijs))))))
      (values (conjunct
               (at-least-one-queen-per-row n)
               (at-most-one-queen-per-line t n)
               (at-most-one-queen-per-line nil n)
               (at-most-one-queen-per-diagonal t n)
               (at-most-one-queen-per-diagonal nil n))
              *bdd*))))

(test n-queens
  ;; 8-queens has 92 solutions
  (multiple-value-bind (u *bdd*)
      (n-queens-formula 8)
    (is (= 92 (sat-count u))))
  ;; 4-queens has 2 solutions
  (multiple-value-bind (u *bdd*)
      (n-queens-formula 4)
    (is (= 2 (sat-count u)))))

(defun full-adder (ci x y)
  (values
   ;; sum
   (xor ci (xor x y))
   ;; carry
   (disjunct
    (conjunct x y)
    (conjunct (xor x y) ci))))

(defun adder-dnf (ci x y)
  (values
   ;; sum
   (disjunct
    (conjunct (negate x) (negate y) ci)
    (conjunct (negate x) y (negate ci))
    (conjunct x (negate y) (negate ci))
    (conjunct x y ci))
   ;; carry
   (disjunct
    (conjunct (negate x) y ci)
    (conjunct x (negate y) ci)
    (conjunct x y (negate ci))
    (conjunct x y ci))))

(test full-adder
  (let* ((*bdd* (make-bdd '(ci x y)))
         (ci (unit 'ci))
         (x  (unit 'x))
         (y  (unit 'y)))
    (multiple-value-bind (s c)
        (full-adder ci x y)
      (multiple-value-bind (dnf-s dnf-c)
          (adder-dnf ci x y)
        (is (eq s dnf-s))
        (is (eq c dnf-c))))))

(defun circuit-1 (x1 y1 x2 y2)
  (conjunct
   (disjunct
    (conjunct x1 y1)
    (nor x1 y1))
   (disjunct
    (conjunct x2 y2)
    (nor x2 y2))))

(defun circuit-2 (x1 y1 x2 y2)
  (disjunct
   (conjunct x1 y2 x2 y1)
   (conjunct x2 y2 (negate x1) (negate y1))
   (conjunct x1 y1 (negate x2) y2)
   (conjunct (negate x2) (negate x1) (negate y1) y2)))

(test circuit-equivalance
  (let* ((*bdd* (make-bdd '(x1 y1 x2 y2)))
         (x1 (unit 'x1))
         (x2 (unit 'x2))
         (y1 (unit 'y1))
         (y2 (unit 'y2)))
    (is (not (eq (circuit-1 x1 y1 x2 y2)
                 (circuit-2 x1 y1 x2 y2))))))

(defun n-bit-adder (carry-in &rest input-variables)
  (labels ((rec (carry-in vars sums)
             (cond ((null vars) (values (reverse sums) carry-in))
                   ((null (cdr vars))
                    (error "odd number of variables"))
                   (t (let ((x (car vars))
                            (y (cadr vars)))
                        (multiple-value-bind (sum carry-out)
                            (full-adder carry-in x y)
                          (rec carry-out (cddr vars) (cons sum sums))))))))
    (rec carry-in input-variables nil)))
