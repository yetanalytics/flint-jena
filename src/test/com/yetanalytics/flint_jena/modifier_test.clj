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
      (is (= [(Var/alloc "v1") (Var/alloc "v2")]
             (->> '[?v1 ?v2]
                  (s/conform ::ms/group-by)
                  (conj [:group-by])
                  (ast/ast->jena {})))))
    (testing "Expression"
      (is (= [(E_Add. (ExprVar. (Var/alloc "v1"))
                      (ExprVar. (Var/alloc "v2")))]
             (->> '[(+ ?v1 ?v2)]
                  (s/conform ::ms/group-by)
                  (conj [:group-by])
                  (ast/ast->jena {})))))
    (testing "Expression as Variable"
      (is (= [(ElementBind. (Var/alloc "v3")
                            (E_Add. (ExprVar. (Var/alloc "v1"))
                                    (ExprVar. (Var/alloc "v2"))))]
             (->> '[[(+ ?v1 ?v2) ?v3]]
                  (s/conform ::ms/group-by)
                  (conj [:group-by])
                  (ast/ast->jena {})))))
    (testing "Combo of all three"
      (is (= [(Var/alloc "v1")
              (E_LogicalAnd. (ExprVar. (Var/alloc "v2"))
                             (ExprVar. (Var/alloc "v3")))
              (ElementBind. (Var/alloc "v5")
                            (E_LogicalNot. (ExprVar. (Var/alloc "v4"))))]
             (->> '[?v1 (and ?v2 ?v3) [(not ?v4) ?v5]]
                  (s/conform ::ms/group-by)
                  (conj [:group-by])
                  (ast/ast->jena {})))))))

(deftest order-by-test
  (testing "ORDER BY -"
    (testing "Variable"
      (is (= [(Var/alloc "v1") (Var/alloc "v2")]
             (->> '[?v1 ?v2]
                  (s/conform ::ms/order-by)
                  (conj [:order-by])
                  (ast/ast->jena {})))))
    (testing "Expression"
      (is (= [(E_Add. (ExprVar. (Var/alloc "v1"))
                      (ExprVar. (Var/alloc "v2")))]
             (->> '[(+ ?v1 ?v2)]
                  (s/conform ::ms/order-by)
                  (conj [:order-by])
                  (ast/ast->jena {}))))
      (is (= [(AggSum. (ExprVar. (Var/alloc "v1")))]
             (->> '[(sum ?v1)]
                  (s/conform ::ms/order-by)
                  (conj [:order-by])
                  (ast/ast->jena {})
                  (map #(.getAggregator ^ExprAggregator %))))))
    (testing "ASC/DESC Expression"
      (is (= [(SortCondition. (E_LogicalNot. (ExprVar. (Var/alloc "v1")))
                              Query/ORDER_ASCENDING)]
             (->> '[(asc (not ?v1))]
                  (s/conform ::ms/order-by)
                  (conj [:order-by])
                  (ast/ast->jena {}))))
      (is (= [(SortCondition. (E_LogicalNot. (ExprVar. (Var/alloc "v1")))
                              Query/ORDER_DESCENDING)]
             (->> '[(desc (not ?v1))]
                  (s/conform ::ms/order-by)
                  (conj [:order-by])
                  (ast/ast->jena {})))))))

(deftest having-test
  (testing "HAVING"
    (is (= [(E_Add. (ExprVar. (Var/alloc "v1"))
                    (ExprVar. (Var/alloc "v2")))]
           (->> '[(+ ?v1 ?v2)]
                (s/conform ::ms/having)
                (conj [:having])
                (ast/ast->jena {}))))
    (is (= [(AggSum. (ExprVar. (Var/alloc "v1")))]
           (->> '[(sum ?v1)]
                (s/conform ::ms/having)
                (conj [:having])
                (ast/ast->jena {})
                (map #(.getAggregator ^ExprAggregator %)))))))

(deftest limit-test
  (testing "LIMIT"
    (is (= 100 (->> 100
                    (s/conform ::ms/limit)
                    (conj [:limit])
                    (ast/ast->jena {}))))))

(deftest offset-test
  (testing "OFFSET"
    (is (= 100 (->> 100
                    (s/conform ::ms/offset)
                    (conj [:offset])
                    (ast/ast->jena {}))))))
