(ns com.yetanalytics.flint-jena.prologue
  (:require [com.yetanalytics.flint-jena.ast :as ast])
  (:import [java.util Map]
           [org.apache.jena.graph Node]
           [org.apache.jena.shared PrefixMapping$Factory]
           [org.apache.jena.sparql.core Prologue]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST -> Jena
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :base [_ [kw ^Node base]]
  [kw (.getURI base)])

(defmethod ast/ast-node->jena :prologue/prefix
  [_ [_ [prefix ^Node iri-node]]]
  [prefix (.getURI iri-node)])

(defmethod ast/ast-node->jena :prefixes [_ [kw prefixes]]
  [kw (doto (PrefixMapping$Factory/create)
        (.setNsPrefixes ^Map (into {} prefixes)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prologue creation + use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti prologue-add! ast/ast-node-dispatch)

(defmethod prologue-add! :default [_ _] nil)

(defmethod prologue-add! :base
  [^Prologue prologue [_ base-ast]]
  (.setBaseURI prologue base-ast))

(defmethod prologue-add! :prefixes
  [^Prologue prologue [_ prefix-ast]]
  (.setPrefixMapping prologue prefix-ast))

(defn create-prologue
  [opts [_ query-update-ast]]
  (let [prologue (Prologue.)]
    (->> query-update-ast
         (filter (fn [[k _]] (#{:base :prefixes} k)))
         (ast/ast->jena opts)
         (run! (fn [ast-node] (prologue-add! prologue ast-node))))
    prologue))

(defn add-prologue!
  [^Prologue query-or-updates ^Prologue prologue]
  (doto query-or-updates
    (.setBase (.getBase prologue))
    (.setPrefixMapping (.getPrefixMapping prologue))))
