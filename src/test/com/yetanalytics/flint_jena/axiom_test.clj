(ns com.yetanalytics.flint-jena.axiom-test
  (:require [clojure.test :refer [deftest testing is]]
            [com.yetanalytics.flint-jena.ast   :as ast]
            [com.yetanalytics.flint-jena.axiom :as ax])
  (:import [org.apache.jena.graph Node]
           [org.apache.jena.sparql.core Prologue Var]
           [org.apache.jena.sparql.lang LabelToNodeMap]))

(def base-prologue
  (doto (Prologue.)
    (.setBaseURI "http://foo.org/")))

(def prefixes-prologue
  (doto (Prologue.)
    (.setPrefix "foo" "http://foo.org/")
    (.setPrefix "" "http://noprefix.org/")))

(deftest axiom-tests
  (testing "Wildcards"
    (is (= :*
           (->> [:ax/wildcard '*]
                (ast/ast->jena {})))))
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
                (catch clojure.lang.ExceptionInfo e
                  (ex-message e))))))
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
    (let [opts {:blank-var-map  (LabelToNodeMap/createVarMap)
                :blank-node-map (LabelToNodeMap/createBNodeMap)}]
      (testing "- blank node vars"
        (is (= true
               (->> [:ax/bnode '_0] ^Var (ast/ast->jena opts) .isBlankNodeVar)
               (->> [:ax/bnode '_] ^Var (ast/ast->jena opts) .isBlankNodeVar)))
        (is (= false
               (->> [:ax/bnode '_0] ^Var (ast/ast->jena opts) .isAnonVar)))
        (is (= true
               (->> [:ax/bnode '_] ^Var (ast/ast->jena opts) .isBlankNodeVar)))
        (is (= "?0"
               (->> [:ax/bnode '_0] ^Var (ast/ast->jena opts) .getName)
               (->> [:ax/bnode '_0] ^Var (ast/ast->jena opts) .getName)))
        (is (not= (->> [:ax/bnode '_0] ^Var (ast/ast->jena opts) .getName)
                  (->> [:ax/bnode '_1] ^Var (ast/ast->jena opts) .getName)))
        (is (not= (->> [:ax/bnode '_] ^Var (ast/ast->jena opts) .getName)
                  (->> [:ax/bnode '_] ^Var (ast/ast->jena opts) .getName))))
      (testing "- raw blank nodes"
        (is (= true
               (->> [:ax/bnode '_0 true] ^Node (ast/ast->jena opts) .isBlank)
               (->> [:ax/bnode '_ true] ^Node (ast/ast->jena opts) .isBlank)))
        (is (= (->> [:ax/bnode '_0 true] ^Node (ast/ast->jena opts) .getBlankNodeLabel)
               (->> [:ax/bnode '_0 true] ^Node (ast/ast->jena opts) .getBlankNodeLabel)))
        (is (not= (->> [:ax/bnode '_ true] ^Node (ast/ast->jena opts) .getBlankNodeLabel)
                  (->> [:ax/bnode '_ true] ^Node (ast/ast->jena opts) .getBlankNodeLabel)))))))

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
    (is (= 3
           (->> [:ax/literal 3.0]
                ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map})
                .getLiteralValue)))
    (is (= "1970-01-01T00:00:00Z"
           (->> [:ax/literal (java.time.Instant/EPOCH)]
                ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map})
                .getLiteralLexicalForm)))
    (testing "- differences between datatype maps"
      (is (= "http://www.w3.org/2001/XMLSchema#integer"
             (->> [:ax/literal 200]
                  ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map})
                  .getLiteralDatatypeURI)))
      (is (= "http://www.w3.org/2001/XMLSchema#long"
             (->> [:ax/literal 200]
                  ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map*})
                  .getLiteralDatatypeURI)))
      (is (= "http://www.w3.org/2001/XMLSchema#decimal"
             (->> [:ax/literal 3.0]
                  ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map})
                  .getLiteralDatatypeURI)))
      (is (= "http://www.w3.org/2001/XMLSchema#double"
             (->> [:ax/literal 3.0]
                  ^Node (ast/ast->jena {:iri->datatype ax/xsd-datatype-map*})
                  .getLiteralDatatypeURI)))))
  (testing "Numeric Literal"
    (is (= 200
           (->> [:ax/numeric 200]
                (ast/ast->jena {}))))))
