(ns com.yetanalytics.flint-jena.prologue
  (:require [com.yetanalytics.flint-jena.ast :as ast])
  (:import [java.util Map]
           [org.apache.jena.graph Node]
           [org.apache.jena.shared PrefixMapping$Factory]
           [org.apache.jena.sparql.core Prologue]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST -> Jena
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :base [_ [_ ^Node base]]
  (.getURI base))

(defmethod ast/ast-node->jena :prologue/prefix
  [_ [_ [prefix ^Node iri-node]]]
  [prefix (.getURI iri-node)])

(defmethod ast/ast-node->jena :prefixes [_ [_ prefixes]]
  (doto (PrefixMapping$Factory/create)
    (.setNsPrefixes ^Map (into {} prefixes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prologue creation + use
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti prologue-add! ast/ast-node-dispatch)

(defmethod prologue-add! :default [_ _ _] nil)

(defmethod prologue-add! :base
  [^Prologue prologue opts base-ast]
  (.setBaseURI prologue (ast/ast->jena opts base-ast)))

(defmethod prologue-add! :prefixes
  [^Prologue prologue opts prefix-ast]
  (.setPrefixMapping prologue (ast/ast->jena opts prefix-ast)))

(defn create-prologue
  [opts [_ query-update-ast]]
  (let [prologue (Prologue.)]
    (run! (fn [ast-node] (prologue-add! prologue opts ast-node))
          query-update-ast)
    prologue))

(defn add-prologue!
  [^Prologue query-or-updates ^Prologue prologue]
  (doto query-or-updates
    (.setBase (.getBase prologue))
    (.setPrefixMapping (.getPrefixMapping prologue))))
