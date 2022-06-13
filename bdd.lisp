(defpackage #:bdd
  (:use #:cl)
  (:export
   #:make-bdd
   #:bdd
   #:*bdd*
   ;;
   #:variable-name
   ;;
   #:ite
   #:ite-constant
   #:nor
   #:nand
   #:xor
   #:xnor
   #:negate
   #:conjunct
   #:disjunct
   #:exists
   #:forall
   ;;
   #:unit
   #:var
   #:true
   #:false
   ;;
   #:sat-count
   #:any-sat
   #:all-sat
   ;;
   #:simplify)
  (:documentation "A small package of classic BDD algorithms."))

(in-package #:bdd)

(defun >0 (x) (> x 0))

(deftype var ()
  "Variables in a NODE are represented by their indices, as positive fixnums."
  '(and fixnum (satisfies >0)))

(defstruct (node (:constructor make-node (var hi lo))
                 (:copier))
  "Node in a binary decision diagram."
  (var 1 :type var :read-only t)
  (hi nil :type (or boolean node) :read-only t)
  (lo nil :type (or boolean node) :read-only t))

(defun leaf-node-p (u)
  "Is U a leaf node?"
  (and (node-p u)
       (eq (node-hi u) (node-lo u))))

(defun make-unique-table ()
  "Create BDD unique table."
  (make-hash-table :test #'equal :weakness :value))

(defun make-computed-table ()
  "Create computed results cache."
  (make-hash-table :test #'equal :weakness :key))

(defstruct (bdd (:constructor %make-bdd (n-vars &aux (n (1+ n-vars)))))
  "Binary decision diagram."
  (n-vars 0 :type fixnum)
  (vars (make-hash-table) :type hash-table)
  (names (make-array n-vars) :type array)
  (unique-table (make-unique-table) :type hash-table)
  (computed-table (make-computed-table) :type hash-table)
  ;; prevent leaves from ever getting GC'd
  (true-node (make-node n t t) :type node)
  (false-node (make-node n nil nil) :type node))

(declaim (type bdd *bdd*))
(defvar *bdd*)

(define-symbol-macro true (bdd-true-node *bdd*))
(define-symbol-macro false (bdd-false-node *bdd*))
(define-symbol-macro computed (bdd-computed-table *bdd*))

(defun variable-index (v)
  "Get variable index for variable V."
  (or (gethash v (bdd-vars *bdd*))
      (error "Unknown variable: ~A" v)))

(defun (setf variable-index) (index v)
  "Set the variable index for variable V."
  (assert (> index 0) (v index) "Index for variable ~A less than 0: ~D" v index)
  (assert (<= index (bdd-n-vars *bdd*))
          (index)
          "Variable ~D is larger than maximum (~D)" index (bdd-n-vars *bdd*))
  (setf (gethash v (bdd-vars *bdd*)) index))

(defun variable-name (index)
  "Get the variable name for the variable at index INDEX."
  (svref (bdd-names *bdd*) (1- index)))

(defun (setf variable-name) (name index)
  "Set the variable name for the variable at index INDEX."
  (setf (svref (bdd-names *bdd*) (1- index)) name))

(defun lookup (var hi lo &aux (k (list var hi lo)))
  "CHeck if a node already exists in the BDD base."
  (declare (dynamic-extent k))
  (gethash k (bdd-unique-table *bdd*)))

(defun insert (u var hi lo &aux (k (list var hi lo)))
  "Insert the node U into the BDD base."
  (setf (gethash k (bdd-unique-table *bdd*)) u))

(defun find-or-add (var hi lo)
  "Insert a node into the BDD base, or return it if it already exists."
  (or (lookup var hi lo)
      (insert (make-node var hi lo) var hi lo)))

(defun make-bdd (order &aux (n (length order)))
  "Construct a BDD for N variables, ordered according to ORDER."
  (let ((*bdd* (%make-bdd n)))
    (let ((i 0))
      (flet ((add-variable (v)
               (incf i)
               (setf (variable-index v) i)
               (setf (variable-name i) v)))
        (map 'nil #'add-variable order)))
    (insert true (1+ n) t t)
    (insert false (1+ n) nil nil)
    *bdd*))

(defun cofactor (f v b &aux (w (node-var f)))
  "Return the top cofactor for the node F, restricting V to value B."
  (declare (type node f) (type var v w) (type boolean b))
  (cond ((leaf-node-p f) f)
        ((< v w) f)
        ((= v w)
         (if b
             (node-hi f)
             (node-lo f)))
        (t (error "node order violated: v = ~a > w ~a" v w))))

(defun top-variable (&rest nodes)
  "Get the lowest index variable from the tops of NODES."
  (apply #'min (mapcar #'node-var nodes)))

(defun ite (f g h &aux (k (list f g h)))
  "IF-THEN-ELSE operator on boolean expressions F, G, and H."
  (declare (type node f g h) (optimize (speed 3)))
  (cond ((and (eq g true) (eq h false)) f)
        ((eq g h) g)
        ((eq f true) g)
        ((eq f false) h)
        ((gethash k computed))
        (t (let* ((v (top-variable f g h))
                  (hi (ite (cofactor f v t)
                           (cofactor g v t)
                           (cofactor h v t)))
                  (lo (ite (cofactor f v nil)
                           (cofactor g v nil)
                           (cofactor h v nil))))
             (if (eq hi lo)
                 hi
                 (setf (gethash k computed)
                       (find-or-add v hi lo)))))))

(defun ite-constant (f g h)
  "Constant-checking ITE constructor for boolean expressions F, G, and H."
  (declare (type node f g h) (optimize (speed 3)))
  (labels ((rec (f g h &aux (k (list f g h)))
             (declare (type (or node keyword) g h))
             (cond ((or (eq g :non-constant)
                        (eq h :non-constant))
                    (return-from ite-constant :non-constant))
                   ((eq g h) g)
                   ((eq f true) g)
                   ((eq f false) h)
                   ((gethash k computed))
                   (t (let* ((v (top-variable f g h))
                             (hi (rec (cofactor f v t)
                                      (cofactor g v t)
                                      (cofactor h v t))))
                        (if (and (not (eq hi true))
                                 (not (eq hi false)))
                            (return-from ite-constant :non-constant)
                            (let ((lo (rec (cofactor f v nil)
                                           (cofactor g v nil)
                                           (cofactor h v nil))))
                              (if (not (eq hi lo))
                                  (return-from ite-constant :non-constant)
                                  (setf (gethash k computed) hi)))))))))
    (rec f g h)))

(defun restrict (f v b &aux (w (node-var f)) (k (list f v b)))
  "Restrict the value of variable V in boolean funcion F to B."
  (declare (type node f) (type var v) (type boolean b))
  (cond ((eq f true) f)
        ((eq f false) f)
        ((< v w) f)
        ((= v w)
         (if b
             (node-hi f)
             (node-lo f)))
        ((gethash k computed))
        (t
         (setf (gethash k computed)
               (find-or-add w
                            (restrict (node-hi f) v b)
                            (restrict (node-lo f) v b))))))

(defun compose (f v g &aux (w (node-var f)))
  "Compose boolean function F and G on variable V."
  (cond ((> w v) f)
        ((= w v) (ite g (node-hi f) (node-lo f)))
        (t (let ((i (compose (node-hi f) v g))
                 (e (compose (node-lo f) v g)))
             (ite f i e)))))

(defun unit (v)
  "Create or return the trivial node for the variable named V."
  (find-or-add (variable-index v) true false))

(defun var (v)
  "Get raw index representation for the variable named V."
  (variable-index v))

(defun negate (u)
  (ite u false true))

(defun conj (u1 u2)
  (ite u1 u2 false))

(defun disj (u1 u2)
  (ite u1 true u2))

(defun xor (u1 u2)
  (ite u1 (negate u2) u2))

(defun nor (u1 u2)
  (ite u1 false (negate u2)))

(defun xnor (u1 u2)
  (ite u1 u2 (negate u2)))

(defun nand (u1 u2)
  (ite u1 (negate u2) true))

(defun conjunct (&rest exprs)
  "Form the conjunction of boolean expressions."
  (reduce #'conj exprs :initial-value true))

(defun disjunct (&rest exprs)
  "Form the disjunction of boolean expressions."
  (reduce #'disj exprs :initial-value false))

(defun quantify (op f x)
  (funcall op
           (restrict f x nil)
           (restrict f x t)))

(defun exists (f x)
  "Existentially quantify boolean function F over variable X."
  (quantify #'disj f x))

(defun forall (f x)
  "Universally quantify boolean function F over variable X."
  (quantify #'conj f x))

(defun evaluate (u env)
  "Evaluate the boolean function U with environment ENV."
  (declare (type node u) (type bit-vector env))
  (cond ((eq u true) 1)
        ((eq u false) 0)
        (t (let ((v (node-var u)))
             (if (= (bit env (1- v)) 1)
                 (evaluate (node-hi u) env)
                 (evaluate (node-lo u) env))))))

(defun sat-count (u)
  "Compute the number of valid truth-assignments for the BDD rooted at U."
  (declare (type node u))
  (let ((counts (make-hash-table)))
    (labels ((rec (u)
               (cond ((eq u false) 0)
                     ((eq u true) 1)
                     ((gethash u counts))
                     (t
                      (let ((lo (rec (node-lo u)))
                            (hi (rec (node-hi u)))
                            (cl (- (node-var (node-lo u)) (node-var u) 1))
                            (ch (- (node-var (node-hi u)) (node-var u) 1)))
                        (setf (gethash u counts)
                              (+ (* (expt 2 cl) lo)
                                 (* (expt 2 ch) hi))))))))
      (* (expt 2 (1- (node-var u))) (rec u)))))

(defun any-sat (u)
  "Find a satisfying truth assignment for the BDD rooted at U, or signal UNSAT."
  (declare (type node u) (optimize (speed 3)))
  (labels ((rec (u)
             (cond ((eq false u)
                    (return-from any-sat :unsat))
                   ((eq true u) nil)
                   ((eq (node-lo u) false)
                    (acons (node-var u) 1
                           (rec (node-hi u))))
                   (t
                    (acons (node-var u) 0
                           (rec (node-lo u)))))))
    (rec u)))

(defun all-sat (u)
  "Return all satisfying truth assignments for the BDD rooted at U."
  (declare (type node u) (optimize (speed 3)))
  (flet ((prepend-binding (u val)
           (lambda (bs)
             (acons (node-var u) val bs))))
    (cond ((eq u true) (list nil))
          ((eq u false) nil)
          (t (append
              (mapcar
               (prepend-binding u 0)
               (all-sat (node-lo u)))
              (mapcar
               (prepend-binding u 1)
               (all-sat (node-hi u))))))))
