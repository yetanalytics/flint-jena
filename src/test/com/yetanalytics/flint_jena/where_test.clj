(ns com.yetanalytics.flint-jena.where-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint-jena]
            [com.yetanalytics.flint-jena.ast   :as ast]
            [com.yetanalytics.flint-jena.axiom :as ax]
            [com.yetanalytics.flint.spec.where :as ws])
  (:import [org.apache.jena.graph Node NodeFactory Triple]
           [org.apache.jena.sparql.core Prologue Var]
           [org.apache.jena.sparql.expr E_Exists]
           [org.apache.jena.sparql.util NodeIsomorphismMap]
           [org.apache.jena.sparql.engine.binding Binding]
           [org.apache.jena.sparql.syntax
            Element
            ElementBind
            ElementData
            ElementFilter
            ElementGroup
            ElementMinus
            ElementNamedGraph
            ElementOptional
            ElementPathBlock
            ElementService
            ElementUnion]))

(def prologue
  (doto (Prologue.)
    (.setPrefix "foo" "http://foo.org/")))

(def ^Node bar-node
  (NodeFactory/createURI "http://foo.org/bar"))

(def ^Triple bar-triple
  (Triple. (NodeFactory/createVariable "s")
           (NodeFactory/createURI "http://foo.org/bar")
           (NodeFactory/createVariable "o")))

(def ^Triple baz-triple
  (Triple. (NodeFactory/createVariable "s")
           (NodeFactory/createURI "http://foo.org/baz")
           (NodeFactory/createVariable "o")))

(def ^Element bar-element
  (let [path-block (doto (ElementPathBlock.)
                     (.addTriple bar-triple))]
    (doto (ElementGroup.)
      (.addElement path-block))))

(def ^Element baz-element
  (let [path-block (doto (ElementPathBlock.)
                     (.addTriple baz-triple))]
    (doto (ElementGroup.)
      (.addElement path-block))))

(deftest where-clause-test
  (testing "WHERE clause"
    (is (.equalTo
         (let [t1 (doto (ElementPathBlock.)
                    (.addTriple bar-triple))
               t2 (doto (ElementPathBlock.)
                    (.addTriple baz-triple))]
           (doto (ElementGroup.)
             (.addElement t1)
             (.addElement t2)))
         (->> '[[?s :foo/bar ?o]
                [?s :foo/baz ?o]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "UNION clause"
    (is (.equalTo
         (let [union (doto (ElementUnion.)
                       (.addElement bar-element)
                       (.addElement baz-element))]
           (doto (ElementGroup.)
             (.addElement union)))
         (->> '[[:union
                 [[?s :foo/bar ?o]]
                 [[?s :foo/baz ?o]]]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "OPTIONAL clause"
    (is (.equalTo
         (let [optional (ElementOptional. bar-element)]
           (doto (ElementGroup.)
             (.addElement optional)))
         (->> '[[:optional
                 [[?s :foo/bar ?o]]]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "MINUS clause"
    (is (.equalTo
         (let [minus (ElementMinus. bar-element)]
           (doto (ElementGroup.)
             (.addElement minus)))
         (->> '[[:minus
                 [[?s :foo/bar ?o]]]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "GRAPH clause"
    (is (.equalTo
         (let [graph (ElementNamedGraph. bar-node baz-element)]
           (doto (ElementGroup.)
             (.addElement graph)))
         (->> '[[:graph :foo/bar
                 [[?s :foo/baz ?o]]]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "SERVICE clause"
    (is (.equalTo
         (let [service (ElementService. bar-node baz-element false)]
           (doto (ElementGroup.)
             (.addElement service)))
         (->> '[[:service :foo/bar
                 [[?s :foo/baz ?o]]]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         (let [service (ElementService. bar-node baz-element true)]
           (doto (ElementGroup.)
             (.addElement service)))
         (->> '[[:service-silent :foo/bar
                 [[?s :foo/baz ?o]]]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "FILTER clause"
    (is (.equalTo
         (let [expr   (E_Exists. baz-element)
               filter (ElementFilter. expr)]
           (doto (ElementGroup.)
             (.addElement filter)))
         (->> '[[:filter (exists [[?s :foo/baz ?o]])]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "BIND clause"
    (is (.equalTo
         (let [var  (Var/alloc "isExists")
               expr (E_Exists. baz-element)
               bind (ElementBind. var expr)]
           (doto (ElementGroup.)
             (.addElement bind)))
         (->> '[[:bind [(exists [[?s :foo/baz ?o]]) ?isExists]]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "VALUES clause"
    (is (.equalTo
         (let [x-var (Var/alloc "x")
               x-val (.build
                      (doto (Binding/builder)
                        (.add x-var bar-node)))
               data   (ElementData. [x-var] [x-val])]
           (doto (ElementGroup.)
             (.addElement data)))
         (->> '[[:values {?x [:foo/bar]}]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue      prologue
                              :iri->datatype ax/xsd-datatype-map}))
         (NodeIsomorphismMap.)))))

;; TODO: Subquery test
