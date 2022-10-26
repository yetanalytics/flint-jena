(ns com.yetanalytics.flint-jena.modifier-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint-jena]
            [com.yetanalytics.flint-jena.ast      :as ast]
            [com.yetanalytics.flint-jena.modifier :as mod]
            [com.yetanalytics.flint.spec.modifier :as ms])
  (:import [org.apache.jena.query Query SortCondition]
           [org.apache.jena.sparql.core Var]
           [org.apache.jena.sparql.expr E_Add E_LogicalAnd E_LogicalNot ExprAggregator ExprVar]
           [org.apache.jena.sparql.expr.aggregate AggSum]
           [org.apache.jena.sparql.syntax ElementBind]))

(deftest group-by-test
  (testing "GROUP BY -"
    (testing "Variable"
      (let [jena-group-by (->> '[?v1 ?v2]
                               (s/conform ::ms/group-by)
                               (conj [:group-by])
                               (ast/ast->jena {}))
            jena-query    (doto (Query.)
                            (mod/add-group-bys! (second jena-group-by)))
            var-list      [(Var/alloc "v1") (Var/alloc "v2")]]
        (is (= [:group-by var-list]
               jena-group-by))
        (is (= var-list
               (.getVars (.getGroupBy jena-query))))
        (is (= {}
               (.getExprs (.getGroupBy jena-query))))))
    (testing "Expression"
      (let [jena-group-by (->> '[(+ ?v1 ?v2)]
                               (s/conform ::ms/group-by)
                               (conj [:group-by])
                               (ast/ast->jena {}))
            jena-query    (doto (Query.)
                            (mod/add-group-bys! (second jena-group-by)))
            expr-list     [(E_Add. (ExprVar. (Var/alloc "v1"))
                                   (ExprVar. (Var/alloc "v2")))]]
        (is (= [:group-by expr-list]
               jena-group-by))
        ;; ?.0 is a dummy variable used to "bind" non-bound exprs
        (is (= [(Var/alloc ".0")]
               (.getVars (.getGroupBy jena-query))))
        (is (= {(Var/alloc ".0") (first expr-list)}
               (.getExprs (.getGroupBy jena-query))))))
    (testing "Expression as Variable"
      (let [jena-group-by (->> '[[(+ ?v1 ?v2) ?v3]]
                               (s/conform ::ms/group-by)
                               (conj [:group-by])
                               (ast/ast->jena {}))
            jena-query    (doto (Query.)
                            (mod/add-group-bys! (second jena-group-by)))
            bind-var      (Var/alloc "v3")
            bind-expr     (E_Add. (ExprVar. (Var/alloc "v1"))
                                  (ExprVar. (Var/alloc "v2")))]
        (is (= [:group-by [(ElementBind. bind-var bind-expr)]]
               jena-group-by))
        (is (= [bind-var]
               (.getVars (.getGroupBy jena-query))))
        (is (= {bind-var bind-expr}
               (.getExprs (.getGroupBy jena-query))))))
    (testing "Combo of all three"
      (let [jena-group-by (->> '[?v1 (and ?v2 ?v3) [(not ?v4) ?v5]]
                               (s/conform ::ms/group-by)
                               (conj [:group-by])
                               (ast/ast->jena {}))
            jena-query    (doto (Query.)
                            (mod/add-group-bys! (second jena-group-by)))
            var-list      [(Var/alloc "v1")
                           (Var/alloc ".0")
                           (Var/alloc "v5")]
            expr-list     [(E_LogicalAnd. (ExprVar. (Var/alloc "v2"))
                                          (ExprVar. (Var/alloc "v3")))
                           (E_LogicalNot. (ExprVar. (Var/alloc "v4")))]]
        (is (= [:group-by [(get var-list 0)
                           (get expr-list 0)
                           (ElementBind. (get var-list 2)
                                         (get expr-list 1))]]
               jena-group-by))
        (is (= var-list
               (.getVars (.getGroupBy jena-query))))
        (is (= {(get var-list 1) (get expr-list 0)
                (get var-list 2) (get expr-list 1)}
               (.getExprs (.getGroupBy jena-query))))))))

(deftest order-by-test
  (testing "ORDER BY -"
    (testing "Variable"
      (let [jena-order-by (->> '[?v1 ?v2]
                               (s/conform ::ms/order-by)
                               (conj [:order-by])
                               (ast/ast->jena {}))
            jena-query    (doto (Query.)
                            (mod/add-order-bys! (second jena-order-by)))]
        (is (= [:order-by [(Var/alloc "v1") (Var/alloc "v2")]]
               jena-order-by))
        (is (= [Query/ORDER_DEFAULT Query/ORDER_DEFAULT]
               (->> (.getOrderBy jena-query)
                    (mapv #(.getDirection ^SortCondition %)))))
        (is (= [(ExprVar. (Var/alloc "v1"))
                (ExprVar. (Var/alloc "v2"))]
               (->> (.getOrderBy jena-query)
                    (mapv #(.getExpression ^SortCondition %)))))))
    (testing "Expression"
      (let [jena-order-by (->> '[(+ ?v1 ?v2)]
                               (s/conform ::ms/order-by)
                               (conj [:order-by])
                               (ast/ast->jena {}))
            jena-query    (doto (Query.)
                            (mod/add-order-bys! (second jena-order-by)))
            order-by-expr (E_Add. (ExprVar. (Var/alloc "v1"))
                                  (ExprVar. (Var/alloc "v2")))]
        (is (= [:order-by [order-by-expr]]
               jena-order-by))
        (is (= [Query/ORDER_DEFAULT]
               (->> (.getOrderBy jena-query)
                    (mapv #(.getDirection ^SortCondition %)))))
        (is (= [order-by-expr]
               (->> (.getOrderBy jena-query)
                    (mapv #(.getExpression ^SortCondition %))))))
      (let [jena-order-by (->> '[(sum ?v1)]
                               (s/conform ::ms/order-by)
                               (conj [:order-by])
                               (ast/ast->jena {:query (Query.)}))
            jena-query    (doto (Query.)
                            (mod/add-order-bys! (second jena-order-by)))
            order-by-agg  (AggSum. (ExprVar. (Var/alloc "v1")))]
        (is (= [:order-by [order-by-agg]]
               (-> jena-order-by
                   (update 1 (fn [exprs] (mapv #(.getAggregator ^ExprAggregator %)
                                               exprs))))))
        (is (= [Query/ORDER_DEFAULT]
               (->> (.getOrderBy jena-query)
                    (mapv #(.getDirection ^SortCondition %)))))
        (is (= [order-by-agg]
               (->> (.getOrderBy jena-query)
                    (mapv #(.getExpression ^SortCondition %))
                    (mapv #(.getAggregator ^ExprAggregator %)))))))
    (testing "ASC/DESC Expression"
      (let [jena-order-by (->> '[(asc (not ?v1))]
                               (s/conform ::ms/order-by)
                               (conj [:order-by])
                               (ast/ast->jena {}))
            jena-query    (doto (Query.)
                            (mod/add-order-bys! (second jena-order-by)))
            order-by-expr (E_LogicalNot. (ExprVar. (Var/alloc "v1")))]
        (is (= [:order-by [(SortCondition. order-by-expr
                                           Query/ORDER_ASCENDING)]]
               jena-order-by))
        (is (= [Query/ORDER_ASCENDING]
               (->> (.getOrderBy jena-query)
                    (mapv #(.getDirection ^SortCondition %)))))
        (is (= [order-by-expr]
               (->> (.getOrderBy jena-query)
                    (mapv #(.getExpression ^SortCondition %))))))
      (let [jena-order-by (->> '[(desc (not ?v1))]
                               (s/conform ::ms/order-by)
                               (conj [:order-by])
                               (ast/ast->jena {}))
            jena-query    (doto (Query.)
                            (mod/add-order-bys! (second jena-order-by)))
            order-by-expr (E_LogicalNot. (ExprVar. (Var/alloc "v1")))]
        
        (is (= [:order-by [(SortCondition. order-by-expr
                                           Query/ORDER_DESCENDING)]]
               jena-order-by))
        (is (= [Query/ORDER_DESCENDING]
               (->> (.getOrderBy jena-query)
                    (mapv #(.getDirection ^SortCondition %)))))
        (is (= [order-by-expr]
               (->> (.getOrderBy jena-query)
                    (mapv #(.getExpression ^SortCondition %)))))))))

(deftest having-test
  (testing "HAVING"
    (let [jena-having (->> '[(+ ?v1 ?v2)]
                           (s/conform ::ms/having)
                           (conj [:having])
                           (ast/ast->jena {:query (Query.)}))
          jena-query  (doto (Query.)
                        (mod/add-having! (second jena-having)))
          having-expr (E_Add. (ExprVar. (Var/alloc "v1"))
                              (ExprVar. (Var/alloc "v2")))]
      (is (= [:having [having-expr]]
             jena-having))
      (is (= [having-expr]
             (.getHavingExprs jena-query))))
    (let [jena-having (->> '[(sum ?v1)]
                           (s/conform ::ms/having)
                           (conj [:having])
                           (ast/ast->jena {:query (Query.)}))
          jena-query  (doto (Query.)
                        (mod/add-having! (second jena-having)))
          having-agg  (AggSum. (ExprVar. (Var/alloc "v1")))]
      (is (= [:having [having-agg]]
             (update jena-having 1 (fn [exprs] (mapv #(.getAggregator ^ExprAggregator %)
                                                     exprs)))))
      (is (= [having-agg]
             (->> (.getHavingExprs jena-query)
                  (mapv #(.getAggregator ^ExprAggregator %))))))))

(deftest limit-test
  (testing "LIMIT"
    (let [jena-limit (->> 100
                          (s/conform ::ms/limit)
                          (conj [:limit])
                          (ast/ast->jena {}))
          jena-query (doto (Query.)
                       (mod/add-limit! (second jena-limit)))]
      
      (is (= [:limit 100]
             jena-limit))
      (is (= 100
             (.getLimit jena-query))))))

(deftest offset-test
  (testing "OFFSET"
    (let [jena-offset (->> 2
                           (s/conform ::ms/offset)
                           (conj [:offset])
                           (ast/ast->jena {}))
          jena-query  (doto (Query.)
                        (mod/add-offset! (second jena-offset)))]
      
      (is (= [:offset 2]
             jena-offset))
      (is (= 2
             (.getOffset jena-query))))))
