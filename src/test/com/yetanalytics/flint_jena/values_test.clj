(ns com.yetanalytics.flint-jena.values-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint-jena]
            [com.yetanalytics.flint-jena.ast    :as ast]
            [com.yetanalytics.flint-jena.axiom  :as ax]
            [com.yetanalytics.flint.spec.values :as vs])
  (:import [com.yetanalytics.flint_jena.values ValueDataBlock]
           [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph NodeFactory]
           [org.apache.jena.sparql.core Prologue Var]
           [org.apache.jena.sparql.engine.binding Binding]))

(def prologue
  (doto (Prologue.)
    (.setPrefix "pre" "http://prefix.org/")))

(deftest values-block-test
  (testing "VALUES block"
    (let [{v1 :variables b1 :values :as db1}
          (->> '{[?foo ?bar ?baz] [[2 nil :pre/one] [nil "x" :pre/two]]}
               (s/conform ::vs/values)
               (ast/ast->jena {:iri->datatype ax/xsd-datatype-map
                               :prologue      prologue}))
          {v2 :variables b2 :values :as db2}
          (->> '{?foo [2 nil] ?bar [nil "x"] ?baz [:pre/one :pre/two]}
               (s/conform ::vs/values)
               (ast/ast->jena {:iri->datatype ax/xsd-datatype-map
                               :prologue      prologue}))
          foo-var
          (Var/alloc "foo")
          bar-var
          (Var/alloc "bar")
          baz-var
          (Var/alloc "baz")
          foo-binding
          (.build
           (doto (Binding/builder)
             (.add foo-var (NodeFactory/createLiteral "2" XSDDatatype/XSDlong))
             (.add baz-var (NodeFactory/createURI "http://prefix.org/one"))))
          bar-binding
          (.build
           (doto (Binding/builder)
             (.add bar-var (NodeFactory/createLiteral "x" XSDDatatype/XSDstring))
             (.add baz-var (NodeFactory/createURI "http://prefix.org/two"))))]
      (is (instance? ValueDataBlock db1))
      (is (instance? ValueDataBlock db2))
      (testing "- variables"
        (is (= #{foo-var bar-var baz-var}
               (set v1)
               (set v2))))
      (testing "- value bindings"
        (is (= #{bar-binding foo-binding}
               (set b1)
               (set b2)))))))
