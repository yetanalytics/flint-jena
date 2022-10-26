(ns com.yetanalytics.flint-jena-test
  (:require [clojure.test                  :refer [deftest testing is]]
            [clojure.edn                   :as edn]
            [clojure.java.io               :as io]
            [com.yetanalytics.flint        :as flint]
            [com.yetanalytics.flint-jena   :refer [create-query
                                                   create-updates
                                                   create-update]])
  (:import [java.io File]
           [org.apache.jena.query QueryFactory]
           [org.apache.jena.update UpdateFactory UpdateRequest]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- format-query
  [query]
  (QueryFactory/create ^String (flint/format-query query)))

(def select-query-fix-1
  '{:prefixes {:foo "<http://foo.org/>"}
    :select   :*
    :from     [:foo/graph]
    :where    [[?x :foo/pred ?z]]})

(def select-query-fix-2
  '{:prefixes {:foo "<http://foo.org/>"}
    :select   [?x [?x ?xx]]
    :from     [:foo/graph]
    :where    [[?x :foo/pred ?z]]
    :group-by [?x]
    :having   [(not= ?x ?z)]})

(def select-distinct-query-fix
  '{:prefixes        {:foo "<http://foo.org/>"}
    :select-distinct [?x ?z ?v]
    :from-named      [:foo/graph]
    :where           [[?x :foo/pred ?z]]
    :values          {?v ["100" "200"]}})

(def select-reduced-query-fix
  '{:prefixes       {:foo "<http://foo.org/>"}
    :select-reduced [?x]
    :from-named     [:foo/graph]
    :where          [[?x :foo/pred {:en "Some Text"}]]
    :order-by       [(asc ?x)]
    :limit          100
    :offset         2})

(def construct-query-fix
  '{:prefixes  {:foo "<http://foo.org/>"}
    :construct [[?x ?y ?z]]
    :from      [:foo/graph]
    :where     [[?x :foo/one ?z]]})

(def describe-query-fix-1
  '{:prefixes   {:foo "<http://foo.org/>"}
    :describe   [?x ?z]
    :from-named [:foo/graph]
    :where      [[?x :foo/pred ?z]]})

(def describe-query-fix-2
  '{:prefixes {:foo "<http://foo.org/>"}
    :describe :*
    :from     [:foo/graph]
    :where    [[?x :foo/qux ?z]]})

(def ask-query-fix
  '{:prefixes   {:foo "<http://foo.org/>"}
    :ask        []
    :from-named [:foo/graph]
    :where      [[?x :foo/pred ?z]]})

(deftest query-create-test
  (testing "SELECT query"
    (is (= (format-query select-query-fix-1)
           (create-query select-query-fix-1)))
    (is (= (format-query select-query-fix-2)
           (create-query select-query-fix-2)))
    (is (= (format-query select-distinct-query-fix)
           (create-query select-distinct-query-fix)))
    (is (= (format-query select-reduced-query-fix)
           (create-query select-reduced-query-fix))))
  (testing "CONSTRUCT query"
    (is (= (format-query construct-query-fix)
           (create-query construct-query-fix))))
  (testing "DESCRIBE query"
    (is (= (format-query describe-query-fix-1)
           (create-query describe-query-fix-1)))
    (is (= (format-query describe-query-fix-2)
           (create-query describe-query-fix-2))))
  (testing "ASK query"
    (is (= (format-query ask-query-fix)
           (create-query ask-query-fix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Update tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- eq-update
  [^UpdateRequest u1 ^UpdateRequest u2]
  (.equalTo u1 u2))

(defn- format-update
  [update]
  (UpdateFactory/create ^String (flint/format-update update)))

(defn- format-updates
  [updates]
  (UpdateFactory/create ^String (flint/format-updates updates)))

(def load-update-fix
  '{:load "<http://foo.org/graph-1>"
    :into [:graph "<http://foo.org/graph-2>"]})

(def load-silent-update-fix
  '{:load-silent "<http://foo.org/graph-1>"
    :into [:graph "<http://foo.org/graph-2>"]})

(def create-update-fix
  '{:create [:graph "<http://foo.org/graph>"]})

(def create-silent-update-fix
  '{:create-silent [:graph "<http://foo.org/graph>"]})

(def clear-update-fix
  '{:clear :default})

(def clear-silent-update-fix
  '{:clear-silent :named})

(def drop-update-fix
  '{:drop :all})

(def drop-silent-update-fix
  '{:drop-silent :all})

(def copy-update-fix
  '{:copy "<http://foo.org/graph-1>"
    :to   "<http://foo.org/graph-2>"})

(def copy-silent-update-fix
  '{:copy-silent "<http://foo.org/graph-1>"
    :to          "<http://foo.org/graph-2>"})

(def move-update-fix
  '{:move "<http://foo.org/graph-1>"
    :to   "<http://foo.org/graph-2>"})

(def move-silent-update-fix
  '{:move-silent "<http://foo.org/graph-1>"
    :to          "<http://foo.org/graph-2>"})

(def add-update-fix
  '{:add "<http://foo.org/graph-1>"
    :to  "<http://foo.org/graph-2>"})

(def add-silent-update-fix
  '{:add-silent "<http://foo.org/graph-1>"
    :to         "<http://foo.org/graph-2>"})

(def insert-data-update-fix
  '{:prefixes    {:foo "<http://foo.org/>"}
    :insert-data [[:foo/subj :foo/pred :foo/obj]]})

(def delete-data-update-fix
  '{:prefixes    {:foo "<http://foo.org/>"}
    :delete-data [[:foo/subj :foo/pred :foo/obj]]})

(def delete-where-update-fix
  '{:prefixes     {:foo "<http://foo.org/>"}
    :delete-where [[:foo/subj ?pred ?obj]]})

(def delete-insert-update-fix-1
  '{:prefixes {:foo "<http://foo.org/>"}
    :delete   [[:foo/subj ?pred ?obj]]
    :insert   [[:foo/subj :foo/pred :foo/obj]]
    :where    [[:foo/subj ?pred ?obj]]
    :using    [:named :foo/graph]})

(def delete-insert-update-fix-2
  '{:prefixes {:foo "<http://foo.org/>"}
    :delete   [[:foo/subj ?pred ?obj]]
    :insert   [[:foo/subj :foo/pred :foo/obj]]
    :where    [[:foo/subj ?pred ?obj]]
    :with     :foo/graph})

(deftest update-create-test
  (testing "LOAD update"
    (is (eq-update (format-update load-update-fix)
                   (create-update load-update-fix)))
    (is (eq-update (format-update load-silent-update-fix)
                   (create-update load-silent-update-fix))))
  (testing "CREATE update"
    (is (eq-update (format-update create-update-fix)
                   (create-update create-update-fix)))
    (is (eq-update (format-update create-silent-update-fix)
                   (create-update create-silent-update-fix))))
  (testing "CLEAR update"
    (is (eq-update (format-update clear-update-fix)
                   (create-update clear-update-fix)))
    (is (eq-update (format-update clear-silent-update-fix)
                   (create-update clear-silent-update-fix))))
  (testing "DROP update"
    (is (eq-update (format-update drop-update-fix)
                   (create-update drop-update-fix)))
    (is (eq-update (format-update drop-silent-update-fix)
                   (create-update drop-silent-update-fix))))
  (testing "COPY update"
    (is (eq-update (format-update copy-update-fix)
                   (create-update copy-update-fix)))
    (is (eq-update (format-update copy-silent-update-fix)
                   (create-update copy-silent-update-fix))))
  (testing "MOVE update"
    (is (eq-update (format-update move-update-fix)
                   (create-update move-update-fix)))
    (is (eq-update (format-update move-silent-update-fix)
                   (create-update move-silent-update-fix))))
  (testing "ADD update"
    (is (eq-update (format-update add-update-fix)
                   (create-update add-update-fix)))
    (is (eq-update (format-update add-silent-update-fix)
                   (create-update add-silent-update-fix))))
  (testing "INSERT DATA update"
    (is (eq-update (format-update insert-data-update-fix)
                   (create-update insert-data-update-fix))))
  (testing "DELETE DATA update"
    (is (eq-update (format-update delete-data-update-fix)
                   (create-update delete-data-update-fix))))
  (testing "DELETE WHERE update"
    (is (eq-update (format-update delete-where-update-fix)
                   (create-update delete-where-update-fix))))
  (testing "DELETE/INSERT update"
    (is (eq-update (format-update delete-insert-update-fix-1)
                   (create-update delete-insert-update-fix-1)))
    (is (eq-update (create-update delete-insert-update-fix-2)
                   (create-update delete-insert-update-fix-2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integration tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- read-files
  [directory]
  (->> directory
       io/file
       file-seq
       (filter #(.isFile ^File %))
       (mapv (fn [f]
               {:name (.getName ^File f)
                :edn  (edn/read-string (slurp f))}))))

(defn- update=
  [update-request-1 update-request-2]
  (.equalTo ^UpdateRequest update-request-1
            ^UpdateRequest update-request-2))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmarking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (require '[criterium.core :as crit])

  (crit/quick-bench
   (format-query select-query-fix-1))
  (crit/quick-bench
   (create-query select-query-fix-1)))
