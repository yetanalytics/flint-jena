(ns com.yetanalytics.flint-jena.triple-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint.spec.triple :as ts]
            [com.yetanalytics.flint-jena.ast    :as ast]
            [com.yetanalytics.flint-jena])
  (:import [org.apache.jena.graph NodeFactory Triple]
           [org.apache.jena.sparql.syntax ElementPathBlock]
           [org.apache.jena.sparql.util NodeIsomorphismMap]))

(defn- make-triple
  ^Triple [svar pvar ovar]
  (Triple/create (NodeFactory/createVariable svar)
                 (NodeFactory/createVariable pvar)
                 (NodeFactory/createVariable ovar)))

(deftest triple-test
  (testing "Triple vector test"
    (is (.equalTo
         (doto (ElementPathBlock.)
           (.addTriple (make-triple "s" "p" "o")))
         (->> '[?s ?p ?o]
              (s/conform ts/triple-vec-spec)
              (conj [:triple/vec])
              (ast/ast->jena {}))
         (NodeIsomorphismMap.))))
  (testing "Normal form test"
    ;; TODO: Make test order-independent
    (is (.equalTo
         (doto (ElementPathBlock.)
           (.addTriple (make-triple "s" "p1" "o1b"))
           (.addTriple (make-triple "s" "p1" "o1a"))
           (.addTriple (make-triple "s" "p2" "o2b"))
           (.addTriple (make-triple "s" "p2" "o2a")))
         (->> '{?s {?p1 #{?o1a ?o1b}
                    ?p2 #{?o2a ?o2b}}}
              (s/conform ts/normal-form-spec)
              (conj [:triple/nform])
              (ast/ast->jena {}))
         (NodeIsomorphismMap.)))))
