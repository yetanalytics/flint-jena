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
            ElementService
            ElementSubQuery
            ElementUnion]))

(defmethod ast/ast-node->jena :where-sub/empty
  [_ _]
  (ElementGroup.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sub-SELECT query
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti select-query-add! ast/ast-node-dispatch)

(defmethod select-query-add! :default [_ _ _] nil)

(defmethod select-query-add! :select
  [^Query query opts select-ast]
  (sel/add-select! query opts select-ast))

(defmethod select-query-add! :select-distinct
  [^Query query opts select-ast]
  (sel/add-select-distinct! query opts select-ast))

(defmethod select-query-add! :select-reduced
  [^Query query opts select-ast]
  (sel/add-select-reduced! query opts select-ast))

(defmethod select-query-add! :group-by
  [query opts group-by-ast]
  (mod/add-group-bys! query opts group-by-ast))

(defmethod select-query-add! :order-by
  [query opts order-by-ast]
  (mod/add-order-bys! query opts order-by-ast))

(defmethod select-query-add! :having
  [query opts having-ast]
  (mod/add-having! query opts having-ast))

(defmethod select-query-add! :limit
  [query opts limit-ast]
  (mod/add-limit! query opts limit-ast))

(defmethod select-query-add! :offset
  [query opts offset-ast]
  (mod/add-offset! query opts offset-ast))

(defmethod select-query-add! :values
  [query opts values-ast]
  (values/add-values! query opts values-ast))

(declare add-where!)

(defmethod select-query-add! :where
  [query opts where-ast]
  (add-where! query opts where-ast))

(defn- create-query
  [{:keys [prologue]} query-ast]
  (let [query (Query. prologue)]
    (.setQuerySelectType query)
    (run! (fn [ast-node] (select-query-add! query ast-node))
          query-ast)
    query))

(defmethod ast/ast-node->jena :where-sub/select
  [opts [_ sub-query]]
  (ElementSubQuery. (create-query opts sub-query)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHERE clauses
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :where-sub/where
  [_ [_ elements]]
  (let [group-element (ElementGroup.)]
    (run! (fn [element] (.addElement group-element element))
          elements)
    group-element))

(defmethod ast/ast-node->jena :where/union
  [_ [_ elements]]
  (let [union-element (ElementUnion.)]
    (run! (fn [element] (.addElement union-element element))
          elements)
    union-element))

(defmethod ast/ast-node->jena :where/recurse
  [_ [_ element]]
  (let [union-element (ElementUnion.)]
    (.addElement union-element element)
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

(defmethod ast/ast-node->jena :where
  [_ [_ where-element]]
  where-element)

(defn add-where! [^Query query opts where-ast]
  (let [where-element (ast/ast->jena opts where-ast)]
    (.setQueryPattern query where-element)))
