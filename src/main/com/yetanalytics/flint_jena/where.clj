(ns com.yetanalytics.flint-jena.where
  (:require [com.yetanalytics.flint-jena.ast      :as ast]
            [com.yetanalytics.flint-jena.modifier :as mod]
            [com.yetanalytics.flint-jena.select   :as sel]
            [com.yetanalytics.flint-jena.values   :as values])
  (:import [org.apache.jena.graph Node]
           [org.apache.jena.query Query]
           [org.apache.jena.sparql.syntax
            Element
            ElementFilter
            ElementGroup
            ElementMinus
            ElementNamedGraph
            ElementOptional
            ElementPathBlock
            ElementService
            ElementSubQuery
            ElementUnion]))

(defmethod ast/ast-node->jena :where [_ where] where)

(defn add-where! [^Query query where-element]
  (.setQueryPattern query where-element))

(defmethod ast/ast-node->jena :where-sub/empty
  [_ _]
  (ElementGroup.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sub-SELECT query
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti select-query-add! ast/ast-node-dispatch)

(defmethod select-query-add! :default [_ _] nil)

(defmethod select-query-add! :select
  [^Query query [_ select-ast]]
  (sel/add-select! query select-ast))

(defmethod select-query-add! :select-distinct
  [^Query query [_ select-ast]]
  (sel/add-select-distinct! query select-ast))

(defmethod select-query-add! :select-reduced
  [^Query query [_ select-ast]]
  (sel/add-select-reduced! query select-ast))

(defmethod select-query-add! :group-by
  [query [_ group-by-ast]]
  (mod/add-group-bys! query group-by-ast))

(defmethod select-query-add! :order-by
  [query [_ order-by-ast]]
  (mod/add-order-bys! query order-by-ast))

(defmethod select-query-add! :having
  [query [_ having-ast]]
  (mod/add-having! query having-ast))

(defmethod select-query-add! :limit
  [query [_ limit-ast]]
  (mod/add-limit! query limit-ast))

(defmethod select-query-add! :offset
  [query [_ offset-ast]]
  (mod/add-offset! query offset-ast))

(defmethod select-query-add! :values
  [query [_ values-ast]]
  (values/add-values! query values-ast))

(defmethod select-query-add! :where
  [query [_ where-ast]]
  (add-where! query where-ast))

(defn- create-query
  [query-ast]
  (let [query (Query.)]
    (.setQuerySelectType query)
    (run! (fn [ast-node] (select-query-add! query ast-node))
          query-ast)
    query))

(defmethod ast/ast-node->jena :where-sub/select
  [_ [_ sub-query]]
  (ElementSubQuery. (create-query sub-query)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHERE clauses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :where/recurse
  [_ [_ element]]
  element)

(defn- path-block? [element]
  (instance? ElementPathBlock element))

(defn- group-path-blocks [pblock-elements]
  (let [acc (ElementPathBlock.)]
    (dorun (for [^ElementPathBlock pb  pblock-elements
                 element-pblock-triple (iterator-seq (.patternElts pb))]
             (.addTriplePath acc element-pblock-triple)))
    [acc]))

(defmethod ast/ast-node->jena :where-sub/where
  [_ [_ elements]]
  (let [elem-partitions (partition-by path-block? elements)
        group-element   (ElementGroup.)]
    (->> elem-partitions
         (mapcat (fn [elem-part]
                   (cond->> elem-part
                     (path-block? (first elem-part)) group-path-blocks)))
         (run! (fn [element]
                 (.addElement group-element element))))
    group-element))

(defmethod ast/ast-node->jena :where/union
  [_ [_ elements]]
  (let [union-element (ElementUnion.)]
    (run! (fn [element] (.addElement union-element element))
          elements)
    union-element))

(defmethod ast/ast-node->jena :where/optional
  [_ [_ element]]
  (ElementOptional. element))

(defmethod ast/ast-node->jena :where/minus
  [_ [_ element]]
  (ElementMinus. element))

(defmethod ast/ast-node->jena :where/graph
  [_ [_ [iri-node element]]]
  (ElementNamedGraph. iri-node element))

(defmethod ast/ast-node->jena :where/service
  [_ [_ [^Node iri-node ^Element element]]]
  (ElementService. iri-node element false))

(defmethod ast/ast-node->jena :where/service-silent
  [_ [_ [^Node iri-node ^Element element]]]
  (ElementService. iri-node element true))

(defmethod ast/ast-node->jena :where/filter
  [_ [_ expr]]
  (ElementFilter. expr))

(defmethod ast/ast-node->jena :where/bind
  [_ [_ bind-element]]
  bind-element)

(defmethod ast/ast-node->jena :where/values
  [_ [_ data-element]]
  data-element)

(defmethod ast/ast-node->jena :where/special
  [_ [_ element]]
  element)
