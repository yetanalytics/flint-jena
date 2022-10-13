(ns com.yetanalytics.flint-jena.where
  (:require [com.yetanalytics.flint-jena.ast :as ast])
  (:import [org.apache.jena.graph Node]
           [org.apache.jena.query Query]
           [org.apache.jena.sparql.syntax
            Element
            ElementBind
            ElementData
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

(defmethod ast/ast-node->jena :where-sub/select
  [_ [_ sub-query]]
  (ElementSubQuery. sub-query))

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
  [_ [_ {:keys [expression variable]}]]
  (ElementBind. variable expression))

(defmethod ast/ast-node->jena :where/values
  [_ [_ {:keys [variables values]}]]
  (ElementData. variables values))

(defmethod ast/ast-node->jena :where/special
  [_ [_ element]]
  element)

(defmethod ast/ast-node->jena :where
  [_ [_ where-element]]
  where-element)

(defn add-where! [^Query query opts where-ast]
  (let [where-element (ast/ast->jena opts where-ast)]
    (.setQueryPattern query where-element)))
