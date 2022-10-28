(ns com.yetanalytics.flint-jena-test
  (:require [clojure.test                  :refer [deftest testing is]]
            [clojure.edn                   :as edn]
            [clojure.java.io               :as io]
            [com.yetanalytics.flint        :as flint]
            [com.yetanalytics.flint-jena   :refer [create-query
                                                   create-updates]])
  (:import [java.io File]
           [org.apache.jena.query QueryFactory]
           [org.apache.jena.update UpdateFactory UpdateRequest]))

(defn- format-query
  [query]
  (QueryFactory/create ^String (flint/format-query query)))

(defn- format-updates
  [updates]
  (UpdateFactory/create ^String (flint/format-updates updates)))

(defn- update=
  [update-request-1 update-request-2]
  (.equalTo ^UpdateRequest update-request-1
            ^UpdateRequest update-request-2))

(defn- read-files
  [directory]
  (->> directory
       io/file
       file-seq
       (filter #(.isFile ^File %))
       (mapv (fn [f]
               {:name (.getName ^File f)
                :edn  (edn/read-string (slurp f))}))))

(defmacro make-query-tests [test-dir]
  `(testing "Jena query from file:"
     ~@(map (fn [{name# :name edn# :edn}]
              `(testing ~name#
                 (is (= (-> (quote ~edn#) format-query)
                        (-> (quote ~edn#) create-query)))))
            (read-files test-dir))))

(defmacro make-update-tests [test-dir]
  `(testing "Jena update from file:"
     ~@(map (fn [{name# :name edn# :edn}]
              `(testing ~name#
                 (is (update= (-> (quote ~edn#) format-updates)
                              (-> (quote ~edn#) create-updates)))))
            (read-files test-dir))))

(deftest query-tests
  (make-query-tests "dev-resources/test-fixtures/query"))

(deftest update-tests
  (make-update-tests "dev-resources/test-fixtures/update"))
