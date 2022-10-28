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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vanilla Flint -> Jena
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- format-query
  [query]
  (QueryFactory/create ^String (flint/format-query query)))

(defn- format-updates
  [updates]
  (UpdateFactory/create ^String (flint/format-updates updates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Read input files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (reduce (fn [acc file-path] (into acc (read-files* file-path)))
          []
          file-paths))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init output files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- make-output-file! [file-path]
  (let [f (io/file file-path)
        d (io/file (.getParent f))]
    (when-let [_ (or (.mkdirs d)
                     (.createNewFile f))]
      (log/infof "Creating result file: %s" file-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compute statistics and table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execute benchmarks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We use quick-benchmark instead of crit/benchmark or else our benching would
;; be super slow without that much gain in precision.

(defn- execute-benches
  [input-maps test-type format-fn create-fn]
  (map (fn [{fname :name fedn :edn}]
         (let [_  (log/infof "Start bench:  %s" fname)
               fr (crit/quick-benchmark (format-fn fedn) {})
               cr (crit/quick-benchmark (create-fn fedn) {})
               _  (log/infof "Finish bench: %s" fname)]
           (result-map fname test-type fr cr)))
       input-maps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output file header
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output file header
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *default-opts*
  "The default opt args map."
  {:query-inputs   ["dev-resources/test-fixtures/query"]
   :query-output   "target/bench/query.txt"
   :update-inputs  ["dev-resources/test-fixtures/update"]
   :update-output  "target/bench/update.txt"})

(defn bench-queries
  "Bench `flint-jena/create-query` against the vanilla Flint querys formatter.
   By default, reads from `dev-resources/test-fixtures/query` and outputs to
   `target/bench/query.txt`."
  [opts]
  (let [opts*   (merge *default-opts* opts)
        queries (read-files (:query-inputs opts*))
        title   query-bench-title
        results (execute-benches queries :query format-query create-query)
        pptab   (partial print-table :query)
        fpath   (:query-output opts*)] 
    (make-output-file! fpath)
    (spit fpath title)
    (spit fpath (with-out-str (pptab results)) :append true)))

(defn bench-updates
  "Bench `flint-jena/create-updates` against the vanilla Flint updates formatter.
   By default, reads from `dev-resources/test-fixtures/update` and outputs to
   `target/bench/update.txt`."
  [opts]
  (let [opts*   (merge *default-opts* opts)
        updates (read-files (:update-inputs opts*))
        title   update-bench-title
        results (execute-benches updates :updates format-updates create-updates)
        pptab   (partial print-table :query)
        fpath   (:update-output opts*)]
    (make-output-file! fpath)
    (spit fpath title)
    (spit fpath (with-out-str (pptab results)) :append true)))

(defn bench
  "Bench both `create-query` and `create-updates` against using vanilla Flint
   and Jena parsing. Accepts the following opt args:
   
   `:query-inputs`  - A vector of file paths from which to read query EDN.
                      Default: `[\"dev-resources/test-fixtures/query\"]`.
   
   `:update-inputs` - A vector of file paths from which to read update EDN.
                      Default: `[\"dev-resources/test-fixtures/query\"]`.
   
   `:query-output`  - A file path string where to write the query bench output.
                      Default: `\"target/bench/query.txt\"`
   
   `:update-output` - A file path string where to write the update bench output.
                      Default: `\"target/bench/update.txt\"`
   "
  [opts]
  (bench-queries opts)
  (bench-updates opts))
