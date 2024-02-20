(ns com.yetanalytics.flint-jena.triple-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint.spec.triple :as ts]
            [com.yetanalytics.flint-jena.ast    :as ast]
            [com.yetanalytics.flint-jena])
  (:import [org.apache.jena.graph NodeFactory Triple]
           [org.apache.jena.sparql.core TriplePath]
           [org.apache.jena.sparql.graph NodeConst]
           [org.apache.jena.sparql.lang LabelToNodeMap]
           [org.apache.jena.sparql.path PathFactory]
           [org.apache.jena.sparql.syntax ElementPathBlock]
           [org.apache.jena.sparql.util NodeIsomorphismMap]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Triple Creation Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-triple
  ^Triple [^String svar ^String pvar ^String ovar]
  (Triple/create (NodeFactory/createVariable svar)
                 (NodeFactory/createVariable pvar)
                 (NodeFactory/createVariable ovar)))

(defn- make-inv-path-triple
  ^TriplePath [^String svar ^String piri ^String ovar]
  (TriplePath. (NodeFactory/createVariable svar)
               (->> piri
                    NodeFactory/createURI
                    PathFactory/pathLink
                    PathFactory/pathInverse)
               (NodeFactory/createVariable ovar)))

(defn- make-bnode-head-triple
  ^Triple [^String bnode ^String pvar ^String ovar]
  (Triple/create (NodeFactory/createBlankNode bnode)
                 (NodeFactory/createVariable pvar)
                 (NodeFactory/createVariable ovar)))

(defn- make-bnode-pointer-triple
  ^Triple [^String svar ^String pvar ^String bnode]
  (Triple/create (NodeFactory/createVariable svar)
                 (NodeFactory/createVariable pvar)
                 (NodeFactory/createBlankNode bnode)))

(defn- make-rdf-first-triple*
  ^Triple [^String bnode ^String bnode-2]
  (Triple/create (NodeFactory/createBlankNode bnode)
                 NodeConst/nodeFirst
                 (NodeFactory/createBlankNode bnode-2)))

(defn- make-rdf-first-triple
  ^Triple [^String bnode ^String ovar]
  (Triple/create (NodeFactory/createBlankNode bnode)
                 NodeConst/nodeFirst
                 (NodeFactory/createVariable ovar)))

(defn- make-rdf-rest-triple
  (^Triple [^String bnode]
   (Triple/create (NodeFactory/createBlankNode bnode)
                  NodeConst/nodeRest
                  NodeConst/nodeNil))
  (^Triple [^String bnode ^String bnode-2]
   (Triple/create (NodeFactory/createBlankNode bnode)
                  NodeConst/nodeRest
                  (NodeFactory/createBlankNode bnode-2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expected Triples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def triples-single
  "```
   ?s ?p ?o .
   ```"
  (doto (ElementPathBlock.)
    (.addTriple (make-triple "s" "p" "o"))))

(def triples-single-path
  "```
   ?s ^<http://foo.org/bar> ?o .
   ```"
  (doto (ElementPathBlock.)
    (.addTriplePath (make-inv-path-triple "s" "http://foo.org/bar" "o"))))

(def triples-multi
  "?s ?p1 ?o1b ;
      ?p1 ?o1a ;
      ?p2 ?o2b ;
      ?p2 ?o2a ."
  (doto (ElementPathBlock.)
    (.addTriple (make-triple "s" "p1" "o1b"))
    (.addTriple (make-triple "s" "p1" "o1a"))
    (.addTriple (make-triple "s" "p2" "o2b"))
    (.addTriple (make-triple "s" "p2" "o2a"))))

(def triples-multi-path
  "```
   ?s ^<http://ex.org/1> ?o1b .
   ?s ^<http://ex.org/1> ?o1a .
   ?s ^<http://ex.org/2> ?o2b .
   ?s ~<http://ex.org/2> ?o2a .
   ```"
  (doto (ElementPathBlock.)
    (.addTriplePath (make-inv-path-triple "s" "http://ex.org/1" "o1b"))
    (.addTriplePath (make-inv-path-triple "s" "http://ex.org/1" "o1a"))
    (.addTriplePath (make-inv-path-triple "s" "http://ex.org/2" "o2b"))
    (.addTriplePath (make-inv-path-triple "s" "http://ex.org/2" "o2a"))))

(def triples-list-subj
  "```
   ( ?s1 ?s2 ) ?p ?o .
   ```
   i.e.
   ```
   _:b0 rdf:first ?s1 ;
        rdf:rest  :_b1 .
   _:b1 rdf:first ?s2 ;
        rdf:rest rdf:nil .
   _:b0 ?p ?o
   ```"
  (doto (ElementPathBlock.)
    (.addTriple (make-rdf-first-triple "_:b0" "s1"))
    (.addTriple (make-rdf-rest-triple "_:b0" "_:b1"))
    (.addTriple (make-rdf-first-triple "_:b1" "s2"))
    (.addTriple (make-rdf-rest-triple "_:b1"))
    (.addTriple (make-bnode-head-triple "_:b0" "p" "o"))))

(def triples-list-obj
  "```
   ?s ?p ?o .
   ```
   i.e.
   ```
   ?s ?p _:b0 .
   _:b0 rdf:first ?s1 ;
        rdf:rest  :_b1 .
   _:b1 rdf:first ?s2 ;
        rdf:rest rdf:nil .
  ```"
  (doto (ElementPathBlock.)
    (.addTriple (make-bnode-pointer-triple "s" "p" "_:b0"))
    (.addTriple (make-rdf-first-triple "_:b0" "o1"))
    (.addTriple (make-rdf-rest-triple "_:b0" "_:b1"))
    (.addTriple (make-rdf-first-triple "_:b1" "o2"))
    (.addTriple (make-rdf-rest-triple "_:b1"))))

(def triples-bnodes-subj
  "```
   [ ?bp ?bq ?br ?bs ] ?p ?o .
   ```
   i.e.
   ```
   _:b0 ?bp ?bq ;
        ?br ?bs ;
        ?p  ?o
   ```"
  (doto (ElementPathBlock.)
    (.addTriple (make-bnode-head-triple "_:b0" "bp" "bq"))
    (.addTriple (make-bnode-head-triple "_:b0" "br" "bs"))
    (.addTriple (make-bnode-head-triple "_:b0" "p" "o"))))

(def triples-bnodes-obj
  "```
   ?s ?p [ ?bp ?bq ?br ?bs ]
   ```
   i.e.
   ```
   ?s ?p _:b0 .
   _:b0 ?bp ?bq ;
        ?br ?bs .
   ```"
  (doto (ElementPathBlock.)
    (.addTriple (make-bnode-pointer-triple "s" "p" "_:b0"))
    (.addTriple (make-bnode-head-triple "_:b0" "bp" "bq"))
    (.addTriple (make-bnode-head-triple "_:b0" "br" "bs"))))

;; NOTE: Jena, using QueryFactory/create, uses pre-order traversal to
;; label the blank nodes, while flint-jena uses post-order traversl. The
;; resulting graphs are isomorphic (which is why tests pass here), but
;; since Jena allocates variable nodes instead of blank nodes for lists and
;; bnode colls, equality checks fail in end-to-end thests.

(def triples-list-bnodes-subj
  "```
   ( ?x [ ?y ?z ] ( ?w ) ) .
   ```
   i.e.
   ```
   _:b0 rdf:first ?x ;
        rdf:rest _:b1 .
   _:b1 rdf:first _:b2 .
   _:b2 ?y ?z .
   _:b1 rdf:rest _:b3 .
   _:b3 rdf:first _:b4 .
   _:b4 rdf:first ?w .
   _:b4 rdf:rest rdf:nil .
   _:b3 rdf:rest rdf:nil .
   ```"
  (doto (ElementPathBlock.)
    (.addTriple (make-rdf-first-triple "_:b0" "x"))
    (.addTriple (make-rdf-rest-triple "_:b0" "_:b1"))
    (.addTriple (make-rdf-first-triple* "_:b1" "_:b2"))
    (.addTriple (make-bnode-head-triple "_:b2" "y" "z"))
    (.addTriple (make-rdf-rest-triple "_:b1" "_:b3"))
    (.addTriple (make-rdf-first-triple* "_:b3" "_:b4"))
    (.addTriple (make-rdf-first-triple "_:b4" "w"))
    (.addTriple (make-rdf-rest-triple "_:b4"))
    (.addTriple (make-rdf-rest-triple "_:b3"))))

(def triples-list-bnodes-obj
  "```
   ?s ?p ( ?x [ ?y ?z ] ( ?w ) ) .
   ```
   i.e.
   ```
   ?s ?p _:b0 .
   _:b0 rdf:first ?x ;
        rdf:rest _:b1 .
   _:b1 rdf:first _:b2 .
   _:b2 ?y ?z .
   _:b1 rdf:rest _:b3 .
   _:b3 rdf:first _:b4 .
   _:b4 rdf:first ?w .
   _:b4 rdf:rest rdf:nil .
   _:b3 rdf:rest rdf:nil .
   ```"
  (doto (ElementPathBlock.)
    (.addTriple (make-bnode-pointer-triple "s" "p" "_:b0"))
    (.addTriple (make-rdf-first-triple "_:b0" "x"))
    (.addTriple (make-rdf-rest-triple "_:b0" "_:b1"))
    (.addTriple (make-rdf-first-triple* "_:b1" "_:b2"))
    (.addTriple (make-bnode-head-triple "_:b2" "y" "z"))
    (.addTriple (make-rdf-rest-triple "_:b1" "_:b3"))
    (.addTriple (make-rdf-first-triple* "_:b3" "_:b4"))
    (.addTriple (make-rdf-first-triple "_:b4" "w"))
    (.addTriple (make-rdf-rest-triple "_:b4"))
    (.addTriple (make-rdf-rest-triple "_:b3"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Triple Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- opt-map []
  {:blank-var-map    (LabelToNodeMap/createVarMap)
   :blank-node-map   (LabelToNodeMap/createBNodeMap)
   ;; This is set to `:blank-var-map` in production, but we set it to
   ;; `:blank-node-map` here since otherwise isomorphic graphs will
   ;; fail the `.equalTo` check.
   :active-bnode-map (atom :blank-node-map)})

(defn triples=
  [^ElementPathBlock expected actual]
  (.equalTo expected
            (->> actual (s/conform ts/triple-spec) (ast/ast->jena (opt-map)))
            (NodeIsomorphismMap.)))

(deftest triple-test
  (testing "Triple vector test"
    (is (triples= triples-single
                  '[?s ?p ?o]))
    (is (triples= triples-single-path
                  '[?s (inv "<http://foo.org/bar>") ?o]))
    (testing "with lists and bnode colls in subject position"
      (is (triples= triples-list-subj
                    '[(?s1 ?s2) ?p ?o]))
      (is (triples= triples-bnodes-subj
                    '[[?bp ?bq ?br ?bs] ?p ?o]))
      (is (triples= triples-list-bnodes-subj
                    '[(?x [?y ?z] (?w))])))
    (testing "with lists and bnode colls in object position"
      (is (triples= triples-list-obj
                    '[?s ?p (?o1 ?o2)]))
      (is (triples= triples-bnodes-obj
                    '[?s ?p [?bp ?bq ?br ?bs]]))
      (is (triples= triples-list-bnodes-obj
                    '[?s ?p (?x [?y ?z] (?w))]))))
  (testing "Normal form test"
    (is (triples= triples-multi
                  '{?s {?p1 #{?o1a ?o1b}
                        ?p2 #{?o2a ?o2b}}}))
    (is (triples= triples-multi-path
                  '{?s {(inv "<http://ex.org/1>") #{?o1a ?o1b}
                        (inv "<http://ex.org/2>") #{?o2a ?o2b}}}))
    (testing "with lists and bnode colls in subject position"
      (is (triples= triples-list-subj
                    '{(?s1 ?s2) {?p #{?o}}}))
      (is (triples= triples-bnodes-subj
                    '{[?bp ?bq ?br ?bs] {?p #{?o}}}))
      (is (triples= triples-list-bnodes-subj
                    '{(?x [?y ?z] (?w)) {}})))
    (testing "with lists and bnode colls in object position"
      (is (triples= triples-list-obj
                    '{?s {?p #{(?o1 ?o2)}}}))
      (is (triples= triples-bnodes-obj
                    '{?s {?p #{[?bp ?bq ?br ?bs]}}}))
      (is (triples= triples-list-bnodes-obj
                    '{?s {?p #{(?x [?y ?z] (?w))}}})))))
