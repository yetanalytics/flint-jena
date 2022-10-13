(ns com.yetanalytics.flint-jena.select
  (:require [com.yetanalytics.flint-jena.ast :as ast])
  (:import [org.apache.jena.graph Node]
           [org.apache.jena.query Query]
           [org.apache.jena.sparql.core Var]
           [org.apache.jena.sparql.expr Expr]
           [org.apache.jena.sparql.syntax ElementBind]))

(defprotocol SelectResult
  (-add-select-result! [res query]))

(extend-protocol SelectResult
  Var
  (-add-select-result! [variable query]
    (.addResultVar ^Query query variable))

  ElementBind
  (-add-select-result! [expr-as-var query]
    (let [^Expr expr (.getExpr expr-as-var)
          ^Node var  (.getVar expr-as-var)]
      (.addResultVar ^Query query var expr))))

(defn- query-add-selects!
  [^Query query select-clauses]
  (if (= :* select-clauses)
    (.setQueryResultStar query true)
    (run! #(-add-select-result! % query) select-clauses)))

(defn add-select!
  [^Query query opts select-ast]
  (let [clauses (ast/ast->jena opts select-ast)]
    (query-add-selects! query clauses)))

(defn add-select-distinct!
  [^Query query opts select-ast]
  (let [clauses (ast/ast->jena opts select-ast)]
    (.setDistinct query true)
    (query-add-selects! query clauses)))

(defn add-select-reduced!
  [^Query query opts select-ast]
  (let [clauses (ast/ast->jena opts select-ast)]
    (.setReduced query true)
    (query-add-selects! query clauses)))
