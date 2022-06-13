(defpackage #:bdd-viz
  (:use #:cl)
  (:import-from
   #:bdd
   #:node
   #:leaf-node-p
   #:node-hi
   #:node-lo
   #:node-var
   #:variable-name)
  (:export #:dot-graph)
  (:documentation "Visualization of BDD's using CL-DOT."))

(in-package #:bdd-viz)

(defmethod cl-dot:graph-object-node ((graph (eql :bdd)) (object node))
  (if (leaf-node-p object)
      (make-instance 'cl-dot:node
                     :attributes
                     (list :label (if (node-lo object) "1" "0")
                           :shape :box))
      (make-instance 'cl-dot:node
                     :attributes
                     (list :label (format nil "~D" (variable-name (node-var object)))
                           :shape :circle))))

(defmethod cl-dot:graph-object-points-to ((graph (eql :bdd)) (object node))
  (unless (leaf-node-p object)
    (list (make-instance 'cl-dot:attributed
                         :object (node-hi object)
                         :attributes (list :label "1"))
          (make-instance 'cl-dot:attributed
                         :object (node-lo object)
                         :attributes (list :label "0"
                                           :style :dotted)))))

(defmethod cl-dot:graph-object-node ((graph (eql :zdd)) (object node))
  (if (leaf-node-p object)
      (make-instance 'cl-dot:node
                     :attributes
                     (list :label (if (node-lo object) "⊤" "⊥")
                           :shape :box))
      (make-instance 'cl-dot:node
                     :attributes
                     (list :label (format nil "~D" (variable-name (node-var object)))
                           :shape :circle))))

(defmethod cl-dot:graph-object-points-to ((graph (eql :zdd)) (object node))
  (unless (leaf-node-p object)
    (list (node-hi object)
          (make-instance 'cl-dot:attributed
                         :object (node-lo object)
                         :attributes (list :style :dotted)))))

(defun dot-graph (node output-file &key (type :bdd) (format :png))
  "Generate CL-DOT graph of the BDD rooted at NODE, writing output to OUTPUT-FILE."
  (cl-dot:dot-graph (cl-dot:generate-graph-from-roots type (list node))
                    output-file
                    :format format))
