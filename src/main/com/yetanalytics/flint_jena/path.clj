(ns com.yetanalytics.flint-jena.path
  (:require [com.yetanalytics.flint-jena.ast :as ast])
  (:import [org.apache.jena.sparql.path
            P_Alt
            P_Link
            P_NegPropSet
            P_ReverseLink
            PathFactory]))

(defmethod ast/ast-node->jena :path/op [_ [_ op]] (keyword op))

(defmethod ast/ast-node->jena :path/paths [_ [_ paths]] paths)

(defmethod ast/ast-node->jena :path/terminal [_ [_ terminal]]
  (PathFactory/pathLink terminal))

;; Negated Paths

(defprotocol NegatedPath
  (-add-negated-path! [this negated-prop-set]))

(extend-protocol NegatedPath
  P_Link
  (-add-negated-path! [path ^P_NegPropSet negated-prop-set]
    (.add negated-prop-set path))
  
  P_ReverseLink
  (-add-negated-path! [path ^P_NegPropSet negated-prop-set]
    (.add negated-prop-set path))
  
  P_Alt
  (-add-negated-path! [path negated-prop-set]
    (let [path-left  (.getLeft path)
          path-right (.getRight path)]
      (-add-negated-path! path-left negated-prop-set)
      (-add-negated-path! path-right negated-prop-set))))

;; Varardic Paths

(defn- path-coll->jena
  [path->jena op paths]
  (cond
    (< (count paths) 2)
    (throw (IllegalArgumentException.
            (format "The '%s' path must have at least 2 arguments!" (name op))))
    (= (count paths) 2)
    (path->jena (first paths) (second paths))
    :else ; Make op right-associative (shouldn't matter here)
    (let [jena-path (path-coll->jena path->jena op (rest paths))]
      (path->jena (first paths) jena-path))))

;; Path spec

(defmethod ast/ast-node->jena :path/branch [_ [_ [op [path :as paths]]]]
  (case op
    :alt (path-coll->jena #(PathFactory/pathAlt %1 %2) op paths)
    :cat (path-coll->jena #(PathFactory/pathSeq %1 %2) op paths)
    :inv (PathFactory/pathInverse path)
    :?   (PathFactory/pathZeroOrOne path)
    :*   (PathFactory/pathZeroOrMore1 path)
    :+   (PathFactory/pathOneOrMore1 path)
    :not (let [neg-prop-set (P_NegPropSet.)]
           (-add-negated-path! path neg-prop-set)
           neg-prop-set)))

(comment
  
  (def link-path
    (PathFactory/pathLink
    ;;  (ast/ast->jena {} [:ax/iri "http://foo.org"])
     (ast/ast->jena {} [:ax/prefix-iri :foo/bar])
    ;;  (ast/ast->jena {} [:ax/rdf-type :a])
     ))
  
  (.getNode ^P_Link link-path)
  
  (let [neg-prop-set (P_NegPropSet.)]
    (-add-negated-path! link-path neg-prop-set)
    neg-prop-set)
  
  )
