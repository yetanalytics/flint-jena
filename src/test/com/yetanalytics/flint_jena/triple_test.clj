(ns com.yetanalytics.flint-jena.triple-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint.spec.triple :as ts]
            [com.yetanalytics.flint-jena.ast    :as ast]
            [com.yetanalytics.flint-jena])
  (:import [org.apache.jena.graph NodeFactory Triple]
           [org.apache.jena.sparql.core TriplePath]
           [org.apache.jena.sparql.path PathFactory]
           [org.apache.jena.sparql.syntax ElementPathBlock]
           [org.apache.jena.sparql.util NodeIsomorphismMap]))

(defn- make-triple
  ^Triple [svar pvar ovar]
  (Triple/create (NodeFactory/createVariable svar)
                 (NodeFactory/createVariable pvar)
                 (NodeFactory/createVariable ovar)))

(defn- make-inv-path-triple
  ^TriplePath [svar piri ovar]
  (TriplePath. (NodeFactory/createVariable svar)
               (->> piri
                    NodeFactory/createURI
                    PathFactory/pathLink
                    PathFactory/pathInverse)
               (NodeFactory/createVariable ovar)))

(deftest triple-test
  (testing "Triple vector test"
    (is (.equalTo
         (doto (ElementPathBlock.)
           (.addTriple (make-triple "s" "p" "o")))
         (->> '[?s ?p ?o]
              (s/conform ts/triple-vec-spec)
              (conj [:triple.vec/spo])
              (ast/ast->jena {}))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         (doto (ElementPathBlock.)
           (.addTriplePath (make-inv-path-triple "s" "http://foo.org/bar" "o")))
         (->> '[?s (inv "<http://foo.org/bar>") ?o]
              (s/conform ts/triple-vec-spec)
              (conj [:triple.vec/spo])
              (ast/ast->jena {}))
         (NodeIsomorphismMap.))))
  (testing "Normal form test"
    ;; TODO: Make tests order-independent
    (is (.equalTo
         (doto (ElementPathBlock.)
           (.addTriple (make-triple "s" "p1" "o1b"))
           (.addTriple (make-triple "s" "p1" "o1a"))
           (.addTriple (make-triple "s" "p2" "o2b"))
           (.addTriple (make-triple "s" "p2" "o2a")))
         (->> '{?s {?p1 #{?o1a ?o1b}
                    ?p2 #{?o2a ?o2b}}}
              (s/conform ts/normal-form-spec)
              (conj [:triple.nform/spo])
              (ast/ast->jena {}))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         (doto (ElementPathBlock.)
           (.addTriplePath (make-inv-path-triple "s" "http://ex.org/1" "o1b"))
           (.addTriplePath (make-inv-path-triple "s" "http://ex.org/1" "o1a"))
           (.addTriplePath (make-inv-path-triple "s" "http://ex.org/2" "o2b"))
           (.addTriplePath (make-inv-path-triple "s" "http://ex.org/2" "o2a")))
         (->> '{?s {(inv "<http://ex.org/1>") #{?o1a ?o1b}
                    (inv "<http://ex.org/2>") #{?o2a ?o2b}}}
              (s/conform ts/normal-form-spec)
              (conj [:triple.nform/spo])
              (ast/ast->jena {}))
         (NodeIsomorphismMap.)))))
