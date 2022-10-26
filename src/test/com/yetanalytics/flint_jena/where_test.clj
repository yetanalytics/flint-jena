(ns com.yetanalytics.flint-jena.where-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint-jena]
            [com.yetanalytics.flint-jena.ast   :as ast]
            [com.yetanalytics.flint-jena.axiom :as ax]
            [com.yetanalytics.flint.spec.where :as ws])
  (:import [org.apache.jena.graph Node NodeFactory Triple]
           [org.apache.jena.query QueryFactory]
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
            ElementSubQuery
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
  (testing "WHERE"
    (is (.equalTo
         (ElementGroup.)
         (->> []
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         (let [triples (doto (ElementPathBlock.)
                         (.addTriple bar-triple)
                         (.addTriple baz-triple))]
           (doto (ElementGroup.)
             (.addElement triples)))
         (->> '[[?s :foo/bar ?o]
                [?s :foo/baz ?o]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         (let [t1 (doto (ElementPathBlock.)
                    (.addTriple bar-triple))
               t2 (doto (ElementPathBlock.)
                    (.addTriple baz-triple))
               f  (ElementFilter. (E_Exists. baz-element))]
           (doto (ElementGroup.)
             (.addElement t1)
             (.addElement f)
             (.addElement t2)))
         (->> '[[?s :foo/bar ?o]
                [:filter (exists [[?s :foo/baz ?o]])]
                [?s :foo/baz ?o]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "UNION"
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
  (testing "OPTIONAL"
    (is (.equalTo
         (let [optional (ElementOptional. bar-element)]
           (doto (ElementGroup.)
             (.addElement optional)))
         (->> '[[:optional
                 [[?s :foo/bar ?o]]]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "MINUS"
    (is (.equalTo
         (let [minus (ElementMinus. bar-element)]
           (doto (ElementGroup.)
             (.addElement minus)))
         (->> '[[:minus
                 [[?s :foo/bar ?o]]]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "GRAPH"
    (is (.equalTo
         (let [graph (ElementNamedGraph. bar-node baz-element)]
           (doto (ElementGroup.)
             (.addElement graph)))
         (->> '[[:graph :foo/bar
                 [[?s :foo/baz ?o]]]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "SERVICE (SILENT)"
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
  (testing "FILTER"
    (is (.equalTo
         (let [expr   (E_Exists. baz-element)
               filter (ElementFilter. expr)]
           (doto (ElementGroup.)
             (.addElement filter)))
         (->> '[[:filter (exists [[?s :foo/baz ?o]])]]
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.))))
  (testing "BIND"
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
  (testing "VALUES"
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

(def subquery-fix-1
  (QueryFactory/create
   "SELECT ?x ?y ?z WHERE { ?x ?y ?z }"))

(def subquery-fix-2
  (QueryFactory/create
   "SELECT ?x WHERE { ?x ?y ?z } GROUP BY ?x HAVING (?x = ?z)"))

(def subquery-fix-3
  (QueryFactory/create
   "SELECT DISTINCT ?x WHERE { ?x ?y ?z } ORDER BY ASC (?x) LIMIT 100 OFFSET 2"))

(def subquery-fix-4
  (QueryFactory/create
   "SELECT REDUCED ?x ?d WHERE { ?x ?y ?z } VALUES ?d { <http://foo.org/bar> }"))

(deftest subquery-test
  (testing "Subquery"
    (is (.equalTo
         (ElementSubQuery. subquery-fix-1)
         (->> '{:select [?x ?y ?z]
                :where  [[?x ?y ?z]]}
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         (ElementSubQuery. subquery-fix-2)
         (->> '{:select   [?x]
                :where    [[?x ?y ?z]]
                :group-by [?x]
                :having   [(= ?x ?z)]}
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         (ElementSubQuery. subquery-fix-3)
         (->> '{:select-distinct [?x]
                :where           [[?x ?y ?z]]
                :order-by        [(asc ?x)]
                :limit           100
                :offset          2}
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         (ElementSubQuery. subquery-fix-4)
         (->> '{:select-reduced [?x ?d]
                :where          [[?x ?y ?z]]
                :values         {?d ["<http://foo.org/bar>"]}}
              (s/conform ::ws/where)
              (ast/ast->jena {:prologue prologue}))
         (NodeIsomorphismMap.)))
    (let [subquery (ElementSubQuery. subquery-fix-1)
          triple   (doto (ElementPathBlock.)
                     (.addTriple bar-triple))
          element  (doto (ElementGroup.)
                     (.addElement triple)
                     (.addElement subquery))]
      (is (.equalTo
           element
           (->> '[[?s :foo/bar ?o]
                  [:where {:select [?x ?y ?z]
                           :where  [[?x ?y ?z]]}]]
                (s/conform ::ws/where)
                (ast/ast->jena {:prologue prologue}))
           (NodeIsomorphismMap.))))))
