(ns com.yetanalytics.flint-jena-bench
  (:require [clojure.edn                   :as edn]
            [clojure.java.io               :as io]
            [clojure.math                  :as math]
            [clojure.pprint                :as pp]
            [clojure.tools.logging         :as log]
            [criterium.core                :as crit]
            [com.yetanalytics.flint        :as flint]
            [com.yetanalytics.flint-jena   :refer [create-query
                                                   create-updates]])
  (:import [java.io File]
           [org.apache.jena.query QueryFactory]
           [org.apache.jena.update UpdateFactory]))

(defn- format-query
  [query]
  (QueryFactory/create ^String (flint/format-query query)))

(defn- format-updates
  [updates]
  (UpdateFactory/create ^String (flint/format-updates updates)))

(defn- read-files*
  [file-path]
  (->> file-path
       io/file
       file-seq
       (filter #(.isFile ^File %))
       (mapv (fn [f]
               {:name (.getName ^File f)
                :edn  (edn/read-string (slurp f))}))))

(defn- read-files
  [file-paths]
  (cond
    (< 1 (count file-paths)) []
    (= 1 (count file-paths)) (read-files* (first file-paths))
    :else (reduce (fn [acc file-path] (into acc (read-files* file-path)))
                  []
                  file-paths)))

;; Default is microseconds

(def ^:dynamic *scale-factor* 1e6)
(def ^:dynamic *scale-unit* "µs")

;; Formula from: 
;; https://en.wikipedia.org/wiki/Student%27s_t-test#Equal_sample_sizes_and_variances

;; Note that deg-freedom = 2 * (sample-n - 1)
;; Threshold for deg-freedom = 10, 95% confidence = 2.228

(def sqrt-2 (math/sqrt 2.0))

(defn- result-map
  [file-name test-type res-1 res-2]
  (let [format-k (keyword (format "format-%s" (name test-type)))
        create-k (keyword (format "create-%s" (name test-type)))
        ;; Extract values from result
        mean-1*  (first (:sample-mean res-1))
        mean-2*  (first (:sample-mean res-2))
        var-1*   (first (:sample-variance res-1))
        var-2*   (first (:sample-variance res-2))
        samp-n   (:sample-count res-1) ; assumes equal sample sizes
        ;; Scale values
        mean-1   (* *scale-factor* mean-1*)
        mean-2   (* *scale-factor* mean-2*)
        var-1    (* *scale-factor* var-1*)
        var-2    (* *scale-factor* var-2*)
        ;; Derive additional values
        mean-d   (- mean-1 mean-2)
        mean-p   (* 100 (/ mean-d mean-1))
        se-1     (math/sqrt (/ var-1 samp-n))
        se-2     (math/sqrt (/ var-2 samp-n))
        se-d     (/ (math/hypot var-1 var-2) samp-n)
        t-val    (abs (/ mean-d se-d))]
    {:file       file-name
     format-k    (format "%.2f ± %.2f" mean-1 se-1)
     create-k    (format "%.2f ± %.2f" mean-2 se-2)
     :difference (format "%.2f ± %.2f" mean-d se-d)
     :percent    (format "%.2f%s" mean-p "%")
     :t-value    (format "%.2f" t-val)}))

(defn- print-table
  [test-type res-map]
  (pp/print-table [:file
                   (keyword (format "format-%s" (name test-type)))
                   (keyword (format "create-%s" (name test-type)))
                   :difference
                   :percent
                   :t-value]
                  res-map))

(defn- execute-benches
  [input-maps test-type format-fn create-fn]
  (map (fn [{fname :name fedn :edn}]
         (let [_  (log/infof "Benching: %s" fname)
               fr (crit/quick-benchmark (format-fn fedn) {})
               cr (crit/quick-benchmark (create-fn fedn) {})]
           (result-map fname test-type fr cr)))
       input-maps))

(defn- make-output-file [file-path]
  (let [f (io/file file-path)
        d (io/file (.getParent f))]
    (log/infof "Output results to: %s" file-path)
    (.mkdirs d)
    (.createNewFile f)))

(def padding-stars
  "************************")

(def query-bench-title
  (format "%s Queries creation bench results (in %s) %s\n"
          padding-stars
          *scale-unit*
          padding-stars))

(def update-bench-title
  (format "%s Updates creation bench results (in %s) %s\n"
          padding-stars
          *scale-unit*
          padding-stars))

(def ^:dynamic *default-opts*
  {:query-inputs   ["dev-resources/test-fixtures/query"]
   :query-outputs  ["target/bench/query-bench.txt"]
   :update-inputs  ["dev-resources/test-fixtures/update"]
   :update-outputs ["target/bench/update-bench.txt"]})

(defn bench-queries
  [opts]
  (let [opts*   (merge *default-opts* opts)
        queries (read-files (:query-inputs opts*))
        title   query-bench-title
        results (execute-benches queries :query format-query create-query)
        pptab   (partial print-table :query)
        fpath   (:query-outputs opts*)] 
    (make-output-file fpath)
    (spit fpath title)
    (spit fpath (with-out-str (pptab results)) :append true)))

(defn bench-updates
  [opts]
  (let [opts*   (merge *default-opts* opts)
        updates (read-files (:update-inputs opts*))
        title   update-bench-title
        results (execute-benches updates :updates format-updates create-updates)
        pptab   (partial print-table :query)
        fpath   (:update-outputs opts*)]
    (make-output-file fpath)
    (spit fpath title)
    (spit fpath (with-out-str (pptab results)) :append true)))

(defn bench
  [opts]
  (bench-queries opts)
  (bench-updates opts))
