(ns com.yetanalytics.flint-jena.modifier
  (:require [com.yetanalytics.flint-jena.ast :as ast]
            [com.yetanalytics.flint-jena.expr])
  (:import [org.apache.jena.graph Node]
           [org.apache.jena.query Query SortCondition]
           [org.apache.jena.sparql.core Var]
           [org.apache.jena.sparql.expr Expr]
           [org.apache.jena.sparql.syntax ElementBind]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST sub-nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :mod/op [_ [_ op]]
  (condp #(= %1 %2) op
    'asc  Query/ORDER_ASCENDING
    'desc Query/ORDER_DESCENDING))

(defmethod ast/ast-node->jena :mod/asc-desc-expr [_ [_ expr]] expr)

(defmethod ast/ast-node->jena :mod/asc-desc [_ [_ [^Integer op ^Expr expr]]]
  (SortCondition. expr op))

(defmethod ast/ast-node->jena :mod/group-expr [_ [_ expr]] expr)

(defmethod ast/ast-node->jena :mod/order-expr [_ [_ expr]] expr)

(defmethod ast/ast-node->jena :mod/expr-as-var [_ [_ expr-as-var]] expr-as-var)

(defmethod ast/ast-node->jena :group-by [_ group-bys] group-bys)
(defmethod ast/ast-node->jena :order-by [_ order-bys] order-bys)
(defmethod ast/ast-node->jena :having [_ havings] havings)
(defmethod ast/ast-node->jena :limit [_ limit] limit)
(defmethod ast/ast-node->jena :offset [_ offset] offset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Records + Protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol GroupByClause
  (-add-group-by! [this query]))

(extend-protocol GroupByClause
  Var
  (-add-group-by! [v ^Query query]
    (.addGroupBy query v))

  Expr
  (-add-group-by! [expr ^Query query]
    (.addGroupBy query expr))

  ElementBind
  (-add-group-by! [expr-as-var ^Query query]
    (let [expr (.getExpr expr-as-var)
          var  (.getVar expr-as-var)]
      (.addGroupBy query var expr))))

(defprotocol OrderByClause
  (-add-order-by! [this query]))

(extend-protocol OrderByClause
  Node
  (-add-order-by! [^Node node ^Query query]
    (.addOrderBy query node Query/ORDER_DEFAULT))

  Expr
  (-add-order-by! [^Expr expr ^Query query]
    (.addOrderBy query expr Query/ORDER_DEFAULT))

  SortCondition
  (-add-order-by! [sort-condition ^Query query]
    (.addOrderBy query sort-condition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-group-bys! [query group-bys]
  (run! #(-add-group-by! % query) group-bys))

(defn add-order-bys! [query order-bys]
  (run! #(-add-order-by! % query) order-bys))

(defn add-having! [^Query query havings]
  (run! #(.addHavingCondition query %) havings))

(defn add-limit! [^Query query limit]
  (.setLimit query limit))

(defn add-offset! [^Query query offset]
  (.setOffset query offset))
