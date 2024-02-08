(ns com.yetanalytics.flint-jena.triple
  (:require [com.yetanalytics.flint-jena.ast :as ast])
  (:import [org.apache.jena.graph Node Triple]
           [org.apache.jena.sparql.graph NodeConst]
           [org.apache.jena.sparql.lang LabelToNodeMap]
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

(defprotocol SubjectObject
  "Protocol for triple subjects and objects. This covers RDF Lists,
    blank node collections, and regular nodes."
  (-head-node [subj-obj]
    "Return the Node that external Triples should point to..")
  (-nested-triples [subj-obj]
    "Return any Triples that are nested in the subject or object."))

(extend-protocol SubjectObject
  Node
  (-head-node [node] node)
  (-nested-triples [_] []))

(defrecord RDFList [first-node triples]
  SubjectObject
  (-head-node [_] first-node)
  (-nested-triples [_] (triple-element->seq triples)))

(defrecord BlankNodeColl [subject-node triples]
  SubjectObject
  (-head-node [_] subject-node)
  (-nested-triples [_] (triple-element->seq triples)))

(defprotocol Predicate
  "Protocol for triple predicates. This covers paths and regular nodes."
  (-create-triple [pred subj obj]
    "Create a triple from the subject, predicate, and object"))

(extend-protocol Predicate
  Node
  (-create-triple [p s o]
    (Triple. s p o))

  Path
  (-create-triple [p s o]
    (TriplePath. s p o)))

(defprotocol ASTTriple ; Can't call this "Triple" for obvious reasons
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

(defn- add-triple-coll! [triple-acc triples]
  (dorun (map (fn [triple] (add-triple! triple-acc triple))
              triples)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blank Node + Triple Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def rdf-first NodeConst/nodeFirst)
(def rdf-rest NodeConst/nodeRest)
(def rdf-nil NodeConst/nodeNil)

(defn- new-bnode! ^Node [^LabelToNodeMap bnode-m]
  (.allocNode bnode-m))

(defn- s->triples [s]
  (or (not-empty (-nested-triples s))
      (throw (ex-info "Subject without predicates or objects is not an RDF list or blank node collecton!"
                      {:kind    ::illegal-subject
                       :subject s}))))

(defn- spo->triples [s p o]
  (let [s-node    (-head-node s)
        o-node    (-head-node o)
        s-triples (-nested-triples s)
        o-triples (-nested-triples o)
        triple    (create-triple s-node p o-node)]
    (concat s-triples [triple] o-triples)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST Methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These multimethod dispatches (specifically `:triple.vec/spo` and
;; `triple.nform/spo`) return ElementPathBlocks since that is the most general
;; syntax Element that can be used in all clauses (WHERE, CONSTRUCT, DELETE,
;; INSERT, etc).

(defmethod ast/ast-node->jena :triple/path [_ [_ path]]
  path)

(defmethod ast/ast-node->jena :triple/list
  [{:keys [active-bnode-map] :as opts} [_ list]]
  (let [bnode-m      (get opts @active-bnode-map)
        triple-block (ElementPathBlock.)
        init-node    (new-bnode! bnode-m)]
    (loop [curr-bnode init-node
           next-bnode (new-bnode! bnode-m)
           list       list]
      (if-some [list-entry (first list)]
        (let [node         (-head-node list-entry)
              triples      (-nested-triples list-entry)
              first-triple (create-triple curr-bnode rdf-first node)
              rest-triple  (if (some? (second list))
                             (create-triple curr-bnode rdf-rest next-bnode)
                             (create-triple curr-bnode rdf-rest rdf-nil))]
          (add-triple! triple-block first-triple)
          (add-triple-coll! triple-block triples)
          (add-triple! triple-block rest-triple)
          (recur next-bnode (new-bnode! bnode-m) (rest list)))
        (->RDFList init-node triple-block)))))

(defmethod ast/ast-node->jena :triple/bnodes
  [{:keys [active-bnode-map] :as opts} [_ po-pairs]]
  (let [bnode-m      (get opts @active-bnode-map)
        triple-block (ElementPathBlock.)
        subj-node    (new-bnode! bnode-m)]
    (dorun
     (for [[p o] po-pairs
           :let [o-node    (-head-node o)
                 o-triples (-nested-triples o)
                 triple    (create-triple subj-node p o-node)]]
       (do
         (add-triple! triple-block triple)
         (add-triple-coll! triple-block o-triples))))
    (->BlankNodeColl subj-node triple-block)))

;; Vectors

(defmethod ast/ast-node->jena :triple.vec/spo [_ [_ [s p o]]]
  (let [triple-block (ElementPathBlock.)
        triples      (spo->triples s p o)]
    (add-triple-coll! triple-block triples)
    triple-block))

(defmethod ast/ast-node->jena :triple.vec/s [_ [_ [s]]]
  (let [triple-block (ElementPathBlock.)
        triples      (s->triples s)]
    (add-triple-coll! triple-block triples)
    triple-block))

;; Normal Forms

(defmethod ast/ast-node->jena :triple.nform/spo [_ [_ spo]]
  (let [triple-block (ElementPathBlock.)
        triples      (mapcat
                      (fn [[s po-coll]]
                        (if (empty? po-coll)
                          (s->triples s)
                          (mapcat (fn [[p o]] (spo->triples s p o)) po-coll)))
                      spo)]
    (add-triple-coll! triple-block triples)
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
