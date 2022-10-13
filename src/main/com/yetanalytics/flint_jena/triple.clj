(ns com.yetanalytics.flint-jena.triple
  (:require [com.yetanalytics.flint-jena.ast :as ast])
  (:import [org.apache.jena.graph Node Triple]
           [org.apache.jena.sparql.core TriplePath]
           [org.apache.jena.sparql.path Path]
           [org.apache.jena.sparql.syntax
            ElementPathBlock
            TripleCollector
            TripleCollectorMark]))

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
    (.addTriple coll ^TriplePath (-create-triple p s o)))
   ([p s o ^TripleCollectorMark coll idx]
    (.addTriple coll idx ^TriplePath (-create-triple p s o)))))

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
