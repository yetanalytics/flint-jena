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
            TripleCollector]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol Predicate
  (-create-triple
    [pred subj obj]))

(extend-protocol Predicate
  Node
  (-create-triple [p s o]
    (Triple. s p o))

  Path
  (-create-triple [p s o]
    (TriplePath. s p o)))

(defprotocol ASTTriple
  (-add-triple! [triple triple-acc]))

(extend-protocol ASTTriple
  Triple
  (-add-triple! [triple ^TripleCollector triple-acc]
    (.addTriple triple-acc triple))
  
  TriplePath
  (-add-triple! [triple-path ^TripleCollector triple-acc]
    (.addTriplePath triple-acc triple-path)))

(defn- create-triple [subj pred obj]
  (-create-triple pred subj obj))

(defn- add-triple! [triple-acc triple]
  (-add-triple! triple triple-acc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- s->triples [s]
  (when-not (:triple-coll? (meta s))
    (throw (ex-info "Subject without predicates or objects is not an RDF list or blank node collecton!"
                    {:kind    ::illegal-subject
                     :subject s})))
  (triple-element->seq (second s)))

(defn- spo->triples [s p o]
  (let [s-coll?   (:triple-coll? (meta s))
        o-coll?   (:triple-coll? (meta o))
        s-node    (if s-coll? (first s) s)
        o-node    (if o-coll? (first o) o)
        s-triples (when s-coll? (triple-element->seq (second s)))
        o-triples (when o-coll? (triple-element->seq (second o)))]
    (concat s-triples
            [(create-triple s-node p o-node)]
            o-triples)))

;; These multimethod dispatches (specifically `:triple/vec` and `triple/nform`)
;; return ElementPathBlocks since that is the most general syntax Element that
;; can be used in all clauses (WHERE, CONSTRUCT, DELETE, INSERT, etc).

(defmethod ast/ast-node->jena :triple/path [_ [_ path]]
  path)

;; Vectors

(defmethod ast/ast-node->jena :triple.vec/spo [_ [_ [s p o]]]
  (let [triple-block (ElementPathBlock.)
        triples (spo->triples s p o)]
    (dorun (map (fn [triple]
                  (add-triple! triple-block triple))
                triples))
    triple-block))

(defmethod ast/ast-node->jena :triple.vec/s [_ [_ [s]]]
  (let [triple-block (ElementPathBlock.)
        triples (s->triples s)]
    (dorun (map (fn [triple]
                  (add-triple! triple-block triple))
                triples))
    triple-block))

;; Normal Forms

(defmethod ast/ast-node->jena :triple.nform/spo [_ [_ spo-coll]]
  (let [triple-block (ElementPathBlock.)]
    (->> spo-coll
         (mapcat
          (fn [[s po-coll]]
            (if (empty? po-coll)
              (s->triples s)
              (mapcat (fn [[p o]] (spo->triples s p o)) po-coll))))
         (map
          (fn [triple]
            (add-triple! triple-block triple)))
         dorun)
    triple-block))

(defmethod ast/ast-node->jena :triple.nform/po [_ [_ po]]
  (mapcat (fn [[p o-coll]] (map (fn [o] [p o]) o-coll))
          po))

(defmethod ast/ast-node->jena :triple.nform/po-empty [_ _]
  [])

(defmethod ast/ast-node->jena :triple.nform/o [_ [_ o]]
  o)

;; Quads

(defmethod ast/ast-node->jena :triple.quad/spo
  [_opts [_ triple-elements]]
  (triple-elements->basic-pattern triple-elements))

(defmethod ast/ast-node->jena :triple.quad/gspo
  [_opts [_ [graph-node triple-bgp]]]
  (basic-pattern->quad-pattern triple-bgp graph-node))
