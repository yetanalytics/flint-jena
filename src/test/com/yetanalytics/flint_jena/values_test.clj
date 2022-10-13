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
           [org.apache.jena.sparql.core Var]
           [org.apache.jena.sparql.engine.binding Binding]))

(deftest values-block-test
  (testing "VALUES block"
    (let [{v1 :variables b1 :values :as db1}
          (->> '{[?foo ?bar] [[nil "x"] [2 nil]]}
               (s/conform ::vs/values)
               (ast/ast->jena {:iri->datatype ax/xsd-datatype-map}))
          {v2 :variables b2 :values :as db2}
          (->> '{?foo [nil 2] ?bar ["x" nil]}
               (s/conform ::vs/values)
               (ast/ast->jena {:iri->datatype ax/xsd-datatype-map}))
          foo-var
          (Var/alloc "foo")
          bar-var
          (Var/alloc "bar")
          foo-binding
          (.build
           (doto (Binding/builder)
             (.add foo-var (NodeFactory/createLiteral "2" XSDDatatype/XSDlong))))
          bar-binding
          (.build
           (doto (Binding/builder)
             (.add bar-var (NodeFactory/createLiteral "x" XSDDatatype/XSDstring))))]
      (is (instance? ValueDataBlock db1))
      (is (instance? ValueDataBlock db2))
      (is (= #{foo-var bar-var}
             (set v1)
             (set v2)))
      (is (= #{bar-binding foo-binding}
             (set b1)
             (set b2))))))
