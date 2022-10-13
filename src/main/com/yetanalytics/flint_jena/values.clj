(ns com.yetanalytics.flint-jena.values
  (:require [com.yetanalytics.flint-jena.ast :as ast])
  (:import [org.apache.jena.graph Node]
           [org.apache.jena.query Query]
           [org.apache.jena.sparql.core Var]
           [org.apache.jena.sparql.engine.binding Binding]))

(defrecord ValueDataBlock [variables values])

(defmethod ast/ast-node->jena :values/undef [_ [_ undef]]
  ;; Returns nil/null
  undef)

(defmethod ast/ast-node->jena :values/map [_ [_ [var-nodes value-rows]]]
  (let [variables
        (mapv (fn [^Node v-node] (Var/alloc v-node)) var-nodes)
        value-bindings
        (vec (for [value-row value-rows]
               (let [row-builder (Binding/builder)]
                 (dorun (map (fn [v val-node]
                               (when (some? val-node)
                                 (.add row-builder v val-node)))
                             variables
                             value-row))
                 (.build row-builder))))]
    (->ValueDataBlock variables value-bindings)))

(defmethod ast/ast-node->jena :values [_ [_ values]] values)

(defn add-values!
  [^Query query opts values-ast]
  (let [[_ {:keys [variables values]}] (ast/ast->jena opts values-ast)]
    (.setValuesDataBlock query variables values)))
