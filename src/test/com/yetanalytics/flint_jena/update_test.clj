(ns com.yetanalytics.flint-jena.update-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint-jena]
            [com.yetanalytics.flint-jena.ast    :as ast]
            [com.yetanalytics.flint.spec.update :as us])
  (:import [org.apache.jena.graph Node NodeFactory]
           [org.apache.jena.sparql.core Prologue Quad]
           [org.apache.jena.sparql.modify.request QuadAcc QuadDataAcc Target]))

(def foo-bar-target
  (Target/create (NodeFactory/createURI "http://foo.org/bar")))

(def graph-target
  (Target/create (NodeFactory/createURI "http://graph.org/")))

(deftest graph-management-test
  (testing "Target graphs"
    (is (= Target/DEFAULT
           (ast/ast->jena {} [:update/default :default])))
    (is (= Target/NAMED
           (ast/ast->jena {} [:update/named :named])))
    (is (= Target/ALL
           (ast/ast->jena {} [:update/all :all])))
    (is (= foo-bar-target
           (ast/ast->jena {} [:update/graph-notag
                              [:ax/iri "<http://foo.org/bar>"]])
           (ast/ast->jena {} [:update/graph
                              [:graph [:ax/iri "<http://foo.org/bar>"]]]))))
  (testing "LOAD (SILENT)"
    (is (= "http://graph.org/"
           (->> "<http://graph.org/>"
                (s/conform ::us/load)
                (conj [:load])
                (ast/ast->jena {}))
           (->> "<http://graph.org/>"
                (s/conform ::us/load-silent)
                (conj [:load-silent])
                (ast/ast->jena {}))))
    (is (= (NodeFactory/createURI "http://graph.org/")
           (->> [:graph "<http://graph.org/>"]
                (s/conform ::us/into)
                (conj [:into])
                (ast/ast->jena {})))))
  (testing "CREATE"
    (is (= (NodeFactory/createURI "http://graph.org/")
           (->> [:graph "<http://graph.org/>"]
                (s/conform ::us/create)
                (conj [:create])
                (ast/ast->jena {}))
           (->> [:graph "<http://graph.org/>"]
                (s/conform ::us/create-silent)
                (conj [:create-silent])
                (ast/ast->jena {})))))
  (testing "CLEAR and DROP"
    (is (= Target/DEFAULT
           (->> :default
                (s/conform ::us/clear)
                (conj [:clear])
                (ast/ast->jena {}))
           (->> :default
                (s/conform ::us/clear-silent)
                (conj [:clear-silent])
                (ast/ast->jena {}))))
    (is (= Target/DEFAULT
           (->> :default
                (s/conform ::us/drop)
                (conj [:drop])
                (ast/ast->jena {}))
           (->> :default
                (s/conform ::us/drop-silent)
                (conj [:drop-silent])
                (ast/ast->jena {})))))
  (testing "MOVE, COPY, and ADD"
    (is (= graph-target
           (->> [:graph "<http://graph.org/>"]
                (s/conform ::us/move)
                (conj [:move])
                (ast/ast->jena {}))
           (->> [:graph "<http://graph.org/>"]
                (s/conform ::us/move-silent)
                (conj [:move-silent])
                (ast/ast->jena {}))))
    (is (= graph-target
           (->> [:graph "<http://graph.org/>"]
                (s/conform ::us/copy)
                (conj [:copy])
                (ast/ast->jena {}))
           (->> [:graph "<http://graph.org/>"]
                (s/conform ::us/copy-silent)
                (conj [:copy-silent])
                (ast/ast->jena {}))))
    (is (= graph-target
           (->> [:graph "<http://graph.org/>"]
                (s/conform ::us/add)
                (conj [:add])
                (ast/ast->jena {}))
           (->> [:graph "<http://graph.org/>"]
                (s/conform ::us/add-silent)
                (conj [:add-silent])
                (ast/ast->jena {}))))))

(def prefix-prologue
  (doto (Prologue.)
    (.setPrefix "foo" "http://foo.org/")))

(def data-fixture
  '[[:foo/s :foo/p :foo/o]
    {:foo/a {:foo/b #{:foo/c}}}
    [:graph :foo/graph
     [[:foo/sg :foo/pg :foo/og]
      {:foo/ag {:foo/bg #{:foo/cg}}}]]])

(def quads-fixture
  [(Quad. (NodeFactory/createURI "urn:x-arq:DefaultGraphNode")
          (NodeFactory/createURI "http://foo.org/s")
          (NodeFactory/createURI "http://foo.org/p")
          (NodeFactory/createURI "http://foo.org/o"))
   (Quad. (NodeFactory/createURI "urn:x-arq:DefaultGraphNode")
          (NodeFactory/createURI "http://foo.org/a")
          (NodeFactory/createURI "http://foo.org/b")
          (NodeFactory/createURI "http://foo.org/c"))
   (Quad. (NodeFactory/createURI "http://foo.org/graph")
          (NodeFactory/createURI "http://foo.org/sg")
          (NodeFactory/createURI "http://foo.org/pg")
          (NodeFactory/createURI "http://foo.org/og"))
   (Quad. (NodeFactory/createURI "http://foo.org/graph")
          (NodeFactory/createURI "http://foo.org/ag")
          (NodeFactory/createURI "http://foo.org/bg")
          (NodeFactory/createURI "http://foo.org/cg"))])

(deftest graph-update-test
  (testing "INSERT DATA"
    (is (= quads-fixture
           (->> data-fixture
                (s/conform ::us/insert-data)
                (conj [:insert-data])
                ^QuadDataAcc (ast/ast->jena {:prologue prefix-prologue})
                .getQuads))))
  (testing "DELETE DATA"
    (is (= quads-fixture
           (->> data-fixture
                (s/conform ::us/delete-data)
                (conj [:delete-data])
                ^QuadDataAcc (ast/ast->jena {:prologue prefix-prologue})
                .getQuads))))
  (testing "DELETE WHERE"
    (is (= quads-fixture
           (->> data-fixture
                (s/conform ::us/delete-where)
                (conj [:delete-where])
                ^QuadAcc (ast/ast->jena {:prologue prefix-prologue})
                .getQuads))))
  (testing "DELETE"
    (is (= quads-fixture
           (->> data-fixture
                (s/conform ::us/delete)
                (conj [:delete])
                (ast/ast->jena {:prologue prefix-prologue})))))
  (testing "INSERT"
    (is (= quads-fixture
           (->> data-fixture
                (s/conform ::us/insert)
                (conj [:insert])
                (ast/ast->jena {:prologue prefix-prologue})))))
  (testing "USING"
    (is (= "http://foo.org/bar"
           (->> :foo/bar
                (s/conform ::us/using)
                (conj [:using])
                ^Node (ast/ast->jena {:prologue prefix-prologue})
                .getURI))))
  (testing "WITH"
    (is (= "http://foo.org/bar"
           (->> :foo/bar
                (s/conform ::us/with)
                (conj [:with])
                (ast/ast->jena {:prologue prefix-prologue}))))))
