(ns com.yetanalytics.flint-jena.modifier-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint-jena]
            [com.yetanalytics.flint-jena.ast      :as ast]
            [com.yetanalytics.flint.spec.modifier :as ms])
  (:import [org.apache.jena.query Query SortCondition]
           [org.apache.jena.sparql.core Var]
           [org.apache.jena.sparql.expr E_Add E_LogicalAnd E_LogicalNot ExprAggregator ExprVar]
           [org.apache.jena.sparql.expr.aggregate AggSum]
           [org.apache.jena.sparql.syntax ElementBind]))

(deftest group-by-test
  (testing "GROUP BY -"
    (testing "Variable"
      (is (= [:group-by [(Var/alloc "v1") (Var/alloc "v2")]]
             (->> '[?v1 ?v2]
                  (s/conform ::ms/group-by)
                  (conj [:group-by])
                  (ast/ast->jena {})))))
    (testing "Expression"
      (is (= [:group-by [(E_Add. (ExprVar. (Var/alloc "v1"))
                                 (ExprVar. (Var/alloc "v2")))]]
             (->> '[(+ ?v1 ?v2)]
                  (s/conform ::ms/group-by)
                  (conj [:group-by])
                  (ast/ast->jena {})))))
    (testing "Expression as Variable"
      (is (= [:group-by [(ElementBind. (Var/alloc "v3")
                                       (E_Add. (ExprVar. (Var/alloc "v1"))
                                               (ExprVar. (Var/alloc "v2"))))]]
             (->> '[[(+ ?v1 ?v2) ?v3]]
                  (s/conform ::ms/group-by)
                  (conj [:group-by])
                  (ast/ast->jena {})))))
    (testing "Combo of all three"
      (is (= [:group-by [(Var/alloc "v1")
                         (E_LogicalAnd. (ExprVar. (Var/alloc "v2"))
                                        (ExprVar. (Var/alloc "v3")))
                         (ElementBind. (Var/alloc "v5")
                                       (E_LogicalNot. (ExprVar. (Var/alloc "v4"))))]]
             (->> '[?v1 (and ?v2 ?v3) [(not ?v4) ?v5]]
                  (s/conform ::ms/group-by)
                  (conj [:group-by])
                  (ast/ast->jena {})))))))

(deftest order-by-test
  (testing "ORDER BY -"
    (testing "Variable"
      (is (= [:order-by [(Var/alloc "v1") (Var/alloc "v2")]]
             (->> '[?v1 ?v2]
                  (s/conform ::ms/order-by)
                  (conj [:order-by])
                  (ast/ast->jena {})))))
    (testing "Expression"
      (is (= [:order-by [(E_Add. (ExprVar. (Var/alloc "v1"))
                                 (ExprVar. (Var/alloc "v2")))]]
             (->> '[(+ ?v1 ?v2)]
                  (s/conform ::ms/order-by)
                  (conj [:order-by])
                  (ast/ast->jena {}))))
      (is (= [:order-by [(AggSum. (ExprVar. (Var/alloc "v1")))]]
             (-> (->> '[(sum ?v1)]
                      (s/conform ::ms/order-by)
                      (conj [:order-by])
                      (ast/ast->jena {}))
                 (update 1 (fn [exprs] (map #(.getAggregator ^ExprAggregator %)
                                            exprs)))))))
    (testing "ASC/DESC Expression"
      (is (= [:order-by
              [(SortCondition. (E_LogicalNot. (ExprVar. (Var/alloc "v1")))
                               Query/ORDER_ASCENDING)]]
             (->> '[(asc (not ?v1))]
                  (s/conform ::ms/order-by)
                  (conj [:order-by])
                  (ast/ast->jena {}))))
      (is (= [:order-by
              [(SortCondition. (E_LogicalNot. (ExprVar. (Var/alloc "v1")))
                               Query/ORDER_DESCENDING)]]
             (->> '[(desc (not ?v1))]
                  (s/conform ::ms/order-by)
                  (conj [:order-by])
                  (ast/ast->jena {})))))))

(deftest having-test
  (testing "HAVING"
    (is (= [:having [(E_Add. (ExprVar. (Var/alloc "v1"))
                             (ExprVar. (Var/alloc "v2")))]]
           (->> '[(+ ?v1 ?v2)]
                (s/conform ::ms/having)
                (conj [:having])
                (ast/ast->jena {}))))
    (is (= [:having [(AggSum. (ExprVar. (Var/alloc "v1")))]]
           (-> (->> '[(sum ?v1)]
                    (s/conform ::ms/having)
                    (conj [:having])
                    (ast/ast->jena {}))
               (update 1 (fn [exprs] (map #(.getAggregator ^ExprAggregator %)
                                          exprs))))))))

(deftest limit-test
  (testing "LIMIT"
    (is (= [:limit 100]
           (->> 100
                (s/conform ::ms/limit)
                (conj [:limit])
                (ast/ast->jena {}))))))

(deftest offset-test
  (testing "OFFSET"
    (is (= [:offset 100]
           (->> 100
                (s/conform ::ms/offset)
                (conj [:offset])
                (ast/ast->jena {}))))))
