(ns com.yetanalytics.flint-jena.axiom-test
  (:require [clojure.test :refer [deftest testing is]]
            [com.yetanalytics.flint-jena.ast   :as ast]
            [com.yetanalytics.flint-jena.axiom :as ax])
  (:import [org.apache.jena.graph Node]
           [org.apache.jena.sparql.core Prologue]))

(def base-prologue
  (doto (Prologue.)
    (.setBaseURI "http://foo.org/")))

(def prefixes-prologue
  (doto (Prologue.)
    (.setPrefix "foo" "http://foo.org/")
    (.setPrefix "" "http://noprefix.org/")))

(deftest axiom-tests
  (testing "Full IRIs"
    (is (= "http://foo.org"
           (->> [:ax/iri "<http://foo.org>"]
                ^Node (ast/ast->jena {})
                .getURI)
           (->> [:ax/iri (java.net.URI/create "http://foo.org")]
                ^Node (ast/ast->jena {})
                .getURI)))
    (is (= "http://foo.org/bar"
           (->> [:ax/iri "<bar>"]
                ^Node (ast/ast->jena {:prologue base-prologue})
                .getURI)
           (->> [:ax/iri (java.net.URI/create "bar")]
                ^Node (ast/ast->jena {:prologue base-prologue})
                .getURI))))
  (testing "Prefixed IRIs"
    (is (= "foo"
           (->> [:ax/prefix :foo]
                (ast/ast->jena {:prologue prefixes-prologue}))))
    (is (= "http://foo.org/baz"
           (->> [:ax/prefix-iri :foo/baz]
                ^Node (ast/ast->jena {:prologue prefixes-prologue})
                .getURI)))
    (is (= "http://noprefix.org/xyz"
           (->> [:ax/prefix-iri :xyz]
                ^Node (ast/ast->jena {:prologue prefixes-prologue})
                .getURI)))
    (is (= "Prefixed IRI 'notexists:xyz' does not have prefix in prologue."
           (try (->> [:ax/prefix-iri :notexists/xyz]
                     ^Node (ast/ast->jena {:prologue prefixes-prologue})
                     .getURI)
                (catch IllegalArgumentException e
                  (.getMessage e))))))
  (testing "'a' IRI"
    (is (= "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
           (->> [:ax/rdf-type :a]
                ^Node (ast/ast->jena {:prologue prefixes-prologue})
                .getURI))))
  (testing "Variables"
    (is (= "someVariable"
           (->> [:ax/var '?someVariable]
                ^Node (ast/ast->jena {})
                .getName))))
  (testing "Blank Nodes"
    (is (= "100"
           (->> [:ax/bnode '_100]
                ^Node (ast/ast->jena {})
                .getBlankNodeLabel)))
    (is (uuid? (->> [:ax/bnode '_]
                    ^Node (ast/ast->jena {})
                    .getBlankNodeLabel
                    java.util.UUID/fromString))))
  (testing "Wildcards"
    (is (= :*
           (->> [:ax/wildcard '*]
                (ast/ast->jena {}))))))

(deftest literal-test
  (testing "Literals"
    (is (= "String Literal"
           (->> [:ax/literal "String Literal"]
                ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map})
                .getLiteralValue)))
    (is (= "你好世界"
           (->> [:ax/literal {:zh-CN "你好世界"}]
                ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map})
                .getLiteralValue)))
    (is (= "zh-CN"
           (->> [:ax/literal {:zh-CN "你好世界"}]
                ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map})
                .getLiteralLanguage)))
    (is (= 200
           (->> [:ax/literal 200]
                ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map})
                .getLiteralValue)))
    (is (= "http://www.w3.org/2001/XMLSchema#double"
           (->> [:ax/literal 3.14]
                ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map})
                .getLiteralDatatypeURI)))
    (is (= "1970-01-01T00:00:00Z"
           (->> [:ax/literal (java.time.Instant/EPOCH)]
                ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map})
                .getLiteralLexicalForm))))
  (testing "Numeric Literal"
    (is (= 200
           (->> [:ax/numeric 200]
                (ast/ast->jena {}))))))
