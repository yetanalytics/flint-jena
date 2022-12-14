(ns com.yetanalytics.flint-jena.triple
  (:require [com.yetanalytics.flint-jena.ast :as ast])
  (:import [org.apache.jena.graph Node Triple]
           [org.apache.jena.sparql.path Path]
           [org.apache.jena.sparql.core
            BasicPattern
            Quad
            QuadPattern
            TriplePath]
           [org.apache.jena.sparql.syntax
            ElementPathBlock
            ElementTriplesBlock
            TripleCollector
            TripleCollectorMark]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These multimethod dispatches (specifically `:triple/vec` and `triple/nform`)
;; return ElementPathBlocks since that is the most general syntax Element that
;; can be used in all clauses (WHERE, CONSTRUCT, DELETE, INSERT, etc).

(defmethod ast/ast-node->jena :triple/path [_ [_ path]]
  path)

(defprotocol Predicate
  (-create-triple
    [pred subj obj])
  (-add-triple!
    [pred subj obj triples]
    [pred subj obj triples idx]))

(extend-protocol Predicate
  Node
  (-create-triple
    [p s o]
    (Triple. s p o))
  (-add-triple!
    ([p s o ^TripleCollector coll]
     (.addTriple coll ^Triple (-create-triple p s o)))
    ([p s o ^TripleCollectorMark coll idx]
     (.addTriple coll idx ^Triple (-create-triple p s o))))

  Path
  (-create-triple
   [p s o]
   (TriplePath. s p o))
  (-add-triple!
   ([p s o ^TripleCollector coll]
    (.addTriplePath coll ^TriplePath (-create-triple p s o)))
   ([p s o ^TripleCollectorMark coll idx]
    (.addTriplePath coll idx ^TriplePath (-create-triple p s o)))))

(defmethod ast/ast-node->jena :triple/vec [_ [_ [s p o]]]
  (let [triple-block (ElementPathBlock.)]
    (-add-triple! p s o triple-block)
    triple-block))

(defmethod ast/ast-node->jena :triple/nform [_ [_ nform]]
  (let [triple-block (ElementPathBlock.)]
    (dorun (map-indexed
            (fn [idx [s p o]] (-add-triple! p s o triple-block idx))
            nform))
    triple-block))

(defmethod ast/ast-node->jena :triple/spo [_ [_ spo]]
  (mapcat (fn [[s po-coll]]
            (map (fn [[p o]] [s p o]) po-coll))
          spo))

(defmethod ast/ast-node->jena :triple/po [_ [_ po]]
  (mapcat (fn [[p o-coll]]
            (map (fn [o] [p o]) o-coll))
          po))

(defmethod ast/ast-node->jena :triple/o [_ [_ o]]
  o)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Triple Conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These functions are used for coercion of ElementPathBlocks into objects
;; used in other namespaces.

;; Iterator seq functions

(defn triple-element->seq
  "Convert an ElementPathBlock into a seq of TriplePaths."
  [^ElementPathBlock triple-element]
  (->> triple-element .patternElts iterator-seq))

(defn triple-element->seq*
  "Convert an TriplePathBlock into a seq of Triples."
  [^ElementTriplesBlock triple-element]
  (->> triple-element .patternElts iterator-seq))

(defn basic-pattern->seq
  "Convert a BasicPattern into a seq of Triples."
  [^BasicPattern basic-pattern]
  (->> basic-pattern .iterator iterator-seq))

(defn quad-pattern->seq
  "Convert a QuadPattern into a seq of Quads."
  [^QuadPattern quad-pattern]
  (->> quad-pattern .iterator iterator-seq))

;; TriplePath -> Triple
(defn triple-path->triple
  "Convert a TriplePath into a Triple.
   
   NOTE: Will throw an exception if the predicate is a path."
  [^TriplePath triple-path]
  (.asTriple triple-path))

;; List<ElementPathBlock> -> ElementPathBlock
(defn triple-elements->element
  "Convert an ElementPathBlock coll into a single ElementPathBlock."
  [triple-elements]
  (let [element (ElementPathBlock.)]
    (dorun (for [triple-el triple-elements
                 triple    (triple-element->seq triple-el)]
             (.addTriplePath element triple)))
    element))

;; List<ElementPathBlock> -> BasicPattern
(defn triple-elements->basic-pattern
  "Convert an ElementPathBlock coll into a single BasicPattern.
   
   NOTE: Will throw an exception if any predicates are paths."
  [triple-elements]
  (let [pattern (BasicPattern.)]
    (dorun (for [elem triple-elements
                 path (triple-element->seq elem)
                 :let [triple (triple-path->triple path)]]
             (.add pattern triple)))
    pattern))

;; BasicPattern -> QuadPattern
(defn basic-pattern->quad-pattern
  "Convert a BasicPattern into a QuadPattern."
  [basic-pattern graph-node]
  (let [pattern (QuadPattern.)]
    (dorun (for [triple (basic-pattern->seq basic-pattern)
                 :let   [quad (Quad. graph-node triple)]]
             (.add pattern quad)))
    pattern))
