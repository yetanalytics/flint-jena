(ns com.yetanalytics.flint-jena.select
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
  [^Query query select-clauses]
  (query-add-selects! query select-clauses))

(defn add-select-distinct!
  [^Query query select-clauses]
  (.setDistinct query true)
  (query-add-selects! query select-clauses))

(defn add-select-reduced!
  [^Query query select-clauses]
  (.setReduced query true)
  (query-add-selects! query select-clauses))
