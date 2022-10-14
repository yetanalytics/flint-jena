(ns com.yetanalytics.flint-jena.query-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint-jena]
            [com.yetanalytics.flint-jena.ast    :as ast]
            [com.yetanalytics.flint.spec.query  :as qs]
            [com.yetanalytics.flint.spec.select :as ss])
  (:import [org.apache.jena.graph NodeFactory Triple]
           [org.apache.jena.sparql.core Prologue Var]
           [org.apache.jena.sparql.expr E_Add ExprVar]
           [org.apache.jena.sparql.syntax ElementBind Template]))

(def prefixes-prologue
  (doto (Prologue.)
    (.setPrefix "foo" "http://foo.org/")))

(deftest from-test
  (testing "FROM"
    (is (= (NodeFactory/createURI "http://foo.org/bar")
           (->> "<http://foo.org/bar>"
                (s/conform ::qs/from)
                (conj [:from])
                (ast/ast->jena {}))
           (->> ["<http://foo.org/bar>"]
                (s/conform ::qs/from)
                (conj [:from])
                (ast/ast->jena {}))
           (->> :foo/bar
                (s/conform ::qs/from)
                (conj [:from])
                (ast/ast->jena {:prologue prefixes-prologue})))))
  (testing "FROM NAMED"
    (is (= [(NodeFactory/createURI "http://foo.org/bar")
            (NodeFactory/createURI "http://foo.org/baz")]
           (->> ["<http://foo.org/bar>"
                 :foo/baz]
                (s/conform ::qs/from-named)
                (conj [:from-named])
                (ast/ast->jena {:prologue prefixes-prologue}))))))

(deftest query-test
  (testing "SELECT"
    (is (= [(Var/alloc "x") (Var/alloc "y") (Var/alloc "z")]
           (->> '[?x ?y ?z]
                (s/conform ::ss/select)
                (conj [:select])
                (ast/ast->jena {}))
           (->> '[?x ?y ?z]
                (s/conform ::ss/select-distinct)
                (conj [:select-distinct])
                (ast/ast->jena {}))
           (->> '[?x ?y ?z]
                (s/conform ::ss/select-reduced)
                (conj [:select-reduced])
                (ast/ast->jena {}))))
    (is (= [(Var/alloc "w")
            (ElementBind. (Var/alloc "z")
                          (E_Add. (ExprVar. (Var/alloc "x"))
                                  (ExprVar. (Var/alloc "y"))))]
           (->> '[?w [(+ ?x ?y) ?z]]
                (s/conform ::ss/select)
                (conj [:select])
                (ast/ast->jena {}))
           (->> '[?w [(+ ?x ?y) ?z]]
                (s/conform ::ss/select-distinct)
                (conj [:select-distinct])
                (ast/ast->jena {}))
           (->> '[?w [(+ ?x ?y) ?z]]
                (s/conform ::ss/select-reduced)
                (conj [:select-reduced])
                (ast/ast->jena {}))))
    (is (= :*
           (->> '*
                (s/conform ::ss/select)
                (conj [:select])
                (ast/ast->jena {}))
           (->> '*
                (s/conform ::ss/select-distinct)
                (conj [:select-distinct])
                (ast/ast->jena {}))
           (->> '*
                (s/conform ::ss/select-reduced)
                (conj [:select-reduced])
                (ast/ast->jena {})))))
  (testing "CONSTRUCT"
    (is (= [(Triple. (Var/alloc "x") (Var/alloc "y") (Var/alloc "z"))
            (Triple. (Var/alloc "a") (Var/alloc "b") (Var/alloc "c"))]
           (->> '[[?x ?y ?z]
                  {?a {?b #{?c}}}]
                (s/conform ::qs/construct)
                (conj [:construct])
                ^Template (ast/ast->jena {})
                .getTriples))))
  (testing "DESCRIBE"
    (is (= [(Var/alloc "x")
            (NodeFactory/createURI "http://foo.org/y")
            (NodeFactory/createURI "http://foo.org/z")]
           (->> '[?x "<http://foo.org/y>" :foo/z]
                (s/conform ::qs/describe)
                (conj [:describe])
                (ast/ast->jena {:prologue prefixes-prologue}))))
    (is (= :*
           (->> '*
                (s/conform ::qs/describe)
                (conj [:describe])
                (ast/ast->jena {})))))
  (testing "ASK"
    (is (nil? (->> nil
                   (s/conform ::qs/ask)
                   (conj [:ask])
                   (ast/ast->jena {}))))
    (is (= [] (->> []
                   (s/conform ::qs/ask)
                   (conj [:ask])
                   (ast/ast->jena {}))))))
