(ns com.yetanalytics.flint-jena.values-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint-jena]
            [com.yetanalytics.flint-jena.ast    :as ast]
            [com.yetanalytics.flint-jena.axiom  :as ax]
            [com.yetanalytics.flint.spec.values :as vs])
  (:import [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph NodeFactory]
           [org.apache.jena.sparql.core Prologue Var]
           [org.apache.jena.sparql.engine.binding Binding]
           [org.apache.jena.sparql.syntax ElementData]))

(def prologue
  (doto (Prologue.)
    (.setPrefix "pre" "http://prefix.org/")))

(deftest values-block-test
  (testing "VALUES block"
    (let [^ElementData data-block-1
          (->> '{[?foo ?bar ?baz] [[2 nil :pre/one] [nil "x" :pre/two]]}
               (s/conform ::vs/values)
               (ast/ast->jena {:iri->datatype ax/xsd-datatype-map
                               :prologue      prologue}))
          ^ElementData data-block-2
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
      (testing "- variables"
        (is (= #{foo-var bar-var baz-var}
               (set (.getVars data-block-1))
               (set (.getVars data-block-2)))))
      (testing "- value bindings"
        (is (= #{bar-binding foo-binding}
               (set (.getRows data-block-1))
               (set (.getRows data-block-2))))))))
