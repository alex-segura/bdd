(defsystem #:bdd
  :description "Binary Decision Diagrams"
  :serial t
  :components
  ((:file "bdd")
   (:file "zdd")))

(defsystem #:bdd/viz
  :depends-on (#:cl-dot #:bdd)
  :components
  ((:file "bdd-viz")))

(defsystem #:bdd/test
  :depends-on (#:bdd #:fiveam)
  :components
  ((:file "bdd-test")))
