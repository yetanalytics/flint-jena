(ns com.yetanalytics.flint-jena.prologue-test
  (:require [clojure.test       :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint-jena]
            [com.yetanalytics.flint-jena.prologue :as pro]
            [com.yetanalytics.flint-jena.ast      :as ast]
            [com.yetanalytics.flint.spec.query    :as qs]
            [com.yetanalytics.flint.spec.prologue :as ps])
  (:import [org.apache.jena.shared PrefixMapping]
           [org.apache.jena.sparql.core Prologue]
           [org.apache.jena.query Query]
           [org.apache.jena.update UpdateRequest]))

(deftest prologue-test
  (testing "Prologue BASE"
    (let [base-uri (->> "<http://base-uri.com/>"
                        (s/conform ::ps/base)
                        (conj [:base])
                        (ast/ast->jena {})
                        second)
          prologue (doto (Prologue.)
                     (.setBaseURI base-uri))]
      (is (= "http://base-uri.com/" base-uri))
      (is (= "http://base-uri.com/suffix"
             (-> prologue .getBase (.resolve "suffix") .toString)))))
  (testing "Prologue PREFIX"
    (let [^PrefixMapping pm  (->> {:foo "<http://foo-uri.com/>"
                                   :$   "<http://bar-uri.com/>"}
                                  (s/conform ::ps/prefixes)
                                  (conj [:prefixes])
                                  (ast/ast->jena {})
                                  second)
          ^Prologue prologue (doto (Prologue.)
                               (.setPrefixMapping pm))]
      (is (= {"foo" "http://foo-uri.com/"
              ""    "http://bar-uri.com/"}
             (.getNsPrefixMap pm)))
      (is (= "http://foo-uri.com/"
             (.getNsPrefixURI pm "foo")))
      (is (= "http://bar-uri.com/"
             (.getNsPrefixURI pm "")))
      (is (= "http://foo-uri.com/suffix"
             (-> prologue (.expandPrefixedName "foo:suffix"))))
      (is (= "http://bar-uri.com/suffix"
             (-> prologue (.expandPrefixedName ":suffix"))))))) 

(deftest prologue-creation-test
  (testing "Prologue with BASE and PREFIX"
    (let [^Prologue prologue
          (->> {:base     "<http://base-uri.com/>"
                :prefixes {:foo "<http://foo-uri.com/>"
                           :$   "<http://bar-uri.com/>"}
                :ask      []
                :where    '[[?x ?y ?z]]}
               (s/conform qs/query-spec)
               (pro/create-prologue {}))]
      (is (= "http://base-uri.com/"
             (-> prologue .getBase .toString)))
      (is (= "http://base-uri.com/suffix"
             (-> prologue .getBase (.resolve "suffix") .toString)))
      (is (= "http://foo-uri.com/"
             (-> prologue (.getPrefix "foo"))))
      (is (= "http://bar-uri.com/"
             (-> prologue (.getPrefix ""))))
      (is (= "http://foo-uri.com/suffix"
             (-> prologue (.expandPrefixedName "foo:suffix"))))
      (is (= "http://bar-uri.com/suffix"
             (-> prologue (.expandPrefixedName ":suffix"))))
      ;; Test prologue addition
      (testing "- on query"
        (let [^Query query (pro/add-prologue! (Query.) prologue)]
          (is (= "http://foo-uri.com/"
                 (.getPrefix query "foo")))
          (is (= "http://bar-uri.com/"
                 (.getPrefix query "")))
          (is (= "http://base-uri.com/"
                 (.getBaseURI query)))
          (is (.explicitlySetBaseURI query))))
      (testing "- on update"
        (let [^UpdateRequest updates (pro/add-prologue! (UpdateRequest.)
                                                        prologue)]
          (is (= "http://foo-uri.com/"
                 (.getPrefix updates "foo")))
          (is (= "http://bar-uri.com/"
                 (.getPrefix updates "")))
          (is (= "http://base-uri.com/"
                 (.getBaseURI updates)))
          (is (.explicitlySetBaseURI updates))))))
  (testing "Prologue with BASE only"
    (let [^Prologue prologue
          (->> {:base  "<http://base-uri.com/>"
                :ask   []
                :where '[[?x ?y ?z]]}
               (s/conform qs/query-spec)
               (pro/create-prologue {}))]
      (is (= "http://base-uri.com/" (-> prologue .getBase .toString)))
      (is (nil? (-> prologue (.getPrefix "foo"))))
      (is (nil? (-> prologue (.getPrefix ""))))))
  (testing "Prologue with PREFIX only"
    (let [^Prologue prologue
          (->> {:prefixes {:foo "<http://foo-uri.com/>"
                           :$   "<http://bar-uri.com/>"}
                :ask      []
                :where    '[[?x ?y ?z]]}
               (s/conform qs/query-spec)
               (pro/create-prologue {}))]
      (is (nil? (-> prologue .getBase)))
      (is (= "http://foo-uri.com/" (-> prologue (.getPrefix "foo"))))
      (is (= "http://bar-uri.com/" (-> prologue (.getPrefix "")))))))
