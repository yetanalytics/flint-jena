(ns com.yetanalytics.flint-jena
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.flint.spec.query  :as qs]
            [com.yetanalytics.flint.spec.update :as us]
            [com.yetanalytics.flint.error       :as err]
            [com.yetanalytics.flint-jena.axiom    :as ax]
            [com.yetanalytics.flint-jena.prologue :as pro]
            [com.yetanalytics.flint-jena.query    :as qu]
            [com.yetanalytics.flint-jena.update   :as up]
            [com.yetanalytics.flint-jena.modifier]
            [com.yetanalytics.flint-jena.path]
            [com.yetanalytics.flint-jena.select]
            [com.yetanalytics.flint-jena.triple]
            [com.yetanalytics.flint-jena.values]
            [com.yetanalytics.flint-jena.where]))

(defn- conform-sparql-err-map
  [error-kw error-loc-kws sparql]
  {:kind    error-kw
   :input   sparql
   :clauses error-loc-kws})

(defn- conform-sparql
  ([error-kw spec spec-err? sparql]
   (conform-sparql error-kw spec spec-err? sparql nil))
  ([error-kw spec spec-err? sparql index]
   (let [ast (s/conform spec sparql)]
     (if (= ::s/invalid ast)
       (let [spec-ed (s/explain-data spec sparql)
             err-kws (err/spec-error-keywords spec-ed)
             err-msg (if (some? index)
                       (err/spec-error-msg err-kws index)
                       (err/spec-error-msg err-kws))
             err-map (cond-> (if spec-err?
                               spec-ed
                               (conform-sparql-err-map error-kw err-kws sparql))
                       (some? index) (assoc :index index))]
         (throw (ex-info err-msg err-map)))
       ast))))

(def conform-query
  (partial conform-sparql ::invalid-query qs/query-spec))

(def conform-update
  (partial conform-sparql ::invalid-update us/update-spec))

(defn create-query
  [query & {:keys [spec-ed?]
            :or   {spec-ed? false}
            :as   opts}]
  (let [query-ast (conform-query spec-ed? query)
        prologue  (pro/create-prologue opts query-ast)
        opts*     (merge opts {:prologue      prologue
                               :iri->datatype ax/xsd-datatype-map})]
    (qu/create-query prologue opts* query-ast)))

(defn create-updates
  [updates & {:keys [spec-ed?]
              :or   {spec-ed? false}
              :as   opts}]
  (let [update-asts (map #(conform-update spec-ed? %) updates)
        prologue    (pro/create-prologue opts (first update-asts))
        opts*       (merge opts {:prologue      prologue
                                 :iri->datatype ax/xsd-datatype-map})]
    (up/create-updates prologue opts* update-asts)))

(defn create-update
  [update & {:keys [spec-ed?]
             :or   {spec-ed? false}}]
  (create-updates [update] :spec-ed? spec-ed?))
