(ns com.yetanalytics.flint-jena.path-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint-jena]
            [com.yetanalytics.flint-jena.ast  :as ast]
            [com.yetanalytics.flint.spec.path :as ps])
  (:import [org.apache.jena.graph NodeFactory]
           [org.apache.jena.sparql.core Prologue]
           [org.apache.jena.sparql.util NodeIsomorphismMap]
           [org.apache.jena.sparql.path
            P_Alt
            P_Inverse
            P_OneOrMore1
            P_NegPropSet
            P_Seq
            P_ZeroOrOne
            P_ZeroOrMore1
            PathFactory]))

(def prologue
  (doto (Prologue.)
    (.setPrefix "foo" "http://foo.org/")))

(defn- path->jena-path
  [path]
  (->> path
       (s/conform ::ps/path)
       (ast/ast->jena {:prologue prologue})))

(defn- iris->jena-path
  [path-fn & iris]
  (if (= 1 (count iris))
    (->> iris
         first
         NodeFactory/createURI
         PathFactory/pathLink
         path-fn)
    (->> iris
         (map #(NodeFactory/createURI %))
         (map #(PathFactory/pathLink %))
         (apply path-fn))))

(deftest path-test
  (testing "Alternates path"
    (is (= "The 'alt' path must have at least 2 arguments!"
           (try (path->jena-path '(alt :foo/bar))
                (catch Exception e
                  (.getMessage e)))))
    (is (.equalTo
         ^P_Alt (iris->jena-path #(PathFactory/pathAlt %1 %2)
                                 "http://foo.org/bar"
                                 "http://foo.org/baz")
         ^P_Alt (path->jena-path '(alt :foo/bar :foo/baz))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         (PathFactory/pathAlt
          (iris->jena-path identity
                           "http://foo.org/bar")
          (iris->jena-path #(PathFactory/pathAlt %1 %2)
                           "http://foo.org/baz"
                           "http://foo.org/qux"))
         (path->jena-path '(alt :foo/bar :foo/baz :foo/qux))
         (NodeIsomorphismMap.))))
  (testing "Sequence path"
    (is (= "The 'cat' path must have at least 2 arguments!"
           (try (path->jena-path '(cat :foo/bar))
                (catch Exception e
                  (.getMessage e)))))
    (is (.equalTo
         ^P_Seq (iris->jena-path #(PathFactory/pathSeq %1 %2)
                                 "http://foo.org/bar"
                                 "http://foo.org/baz")
         ^P_Seq (path->jena-path '(cat :foo/bar :foo/baz))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         (PathFactory/pathSeq
          (iris->jena-path identity
                           "http://foo.org/bar")
          (iris->jena-path #(PathFactory/pathSeq %1 %2)
                           "http://foo.org/baz"
                           "http://foo.org/qux"))
         (path->jena-path '(cat :foo/bar :foo/baz :foo/qux))
         (NodeIsomorphismMap.))))
  (testing "Inverse path"
    (is (.equalTo
         ^P_Inverse (iris->jena-path #(PathFactory/pathInverse %)
                                     "http://foo.org/bar")
         ^P_Inverse (path->jena-path '(inv :foo/bar))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         ^P_Inverse (iris->jena-path #(PathFactory/pathInverse %)
                                     "http://foo.org/bar")
         ^P_Inverse (path->jena-path '(inv "<http://foo.org/bar>"))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         ^P_Inverse (iris->jena-path #(PathFactory/pathInverse %)
                                     "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
         ^P_Inverse (path->jena-path '(inv a))
         (NodeIsomorphismMap.))))
  (testing "Zero or One path"
    (is (.equalTo
         ^P_ZeroOrOne (iris->jena-path #(PathFactory/pathZeroOrOne %)
                                       "http://foo.org/bar")
         ^P_ZeroOrOne (path->jena-path '(? :foo/bar))
         (NodeIsomorphismMap.))))
  (testing "Zero or More path"
    (is (.equalTo
         ^P_ZeroOrMore1 (iris->jena-path #(PathFactory/pathZeroOrMore1 %)
                                        "http://foo.org/bar")
         ^P_ZeroOrMore1 (path->jena-path '(* :foo/bar))
         (NodeIsomorphismMap.))))
  (testing "One or More path"
    (is (.equalTo
         ^P_OneOrMore1 (iris->jena-path #(PathFactory/pathOneOrMore1 %)
                                       "http://foo.org/bar")
         ^P_OneOrMore1 (path->jena-path '(+ :foo/bar))
         (NodeIsomorphismMap.))))
  (testing "Negation path"
    (is (.equalTo
         ^P_NegPropSet (iris->jena-path #(doto (P_NegPropSet.)
                                           (.add %))
                                        "http://foo.org/bar")
         ^P_NegPropSet (path->jena-path '(not :foo/bar))
         (NodeIsomorphismMap.)))
    (is (.equalTo
         ^P_NegPropSet (iris->jena-path #(doto (P_NegPropSet.)
                                           (.add %1)
                                           (.add %2))
                                        "http://foo.org/bar"
                                        "http://foo.org/baz")
         ^P_NegPropSet (path->jena-path '(not (alt :foo/bar :foo/baz)))
         (NodeIsomorphismMap.))))
  (testing "Nested paths"
    (is (.equalTo
         (-> (PathFactory/pathAlt
              (iris->jena-path #(PathFactory/pathSeq %1 %2)
                               "http://foo.org/bar"
                               "http://foo.org/baz")
              (iris->jena-path identity
                               "http://foo.org/qux"))
             PathFactory/pathInverse
             PathFactory/pathZeroOrOne
             PathFactory/pathZeroOrMore1
             PathFactory/pathOneOrMore1)
         (path->jena-path
          '(+ (* (? (inv (alt (cat :foo/bar :foo/baz) :foo/qux))))))
         (NodeIsomorphismMap.)))))
