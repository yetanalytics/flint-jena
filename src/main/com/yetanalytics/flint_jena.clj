(ns com.yetanalytics.flint-jena
  (:require [clojure.spec.alpha :as s]
            [com.yetanalytics.flint.spec.query  :as qs]
            [com.yetanalytics.flint.spec.update :as us]
            [com.yetanalytics.flint.error       :as err]
            [com.yetanalytics.flint-jena.axiom    :as ax]
            [com.yetanalytics.flint-jena.prologue :as pro]
            [com.yetanalytics.flint-jena.query    :as qu]
            [com.yetanalytics.flint-jena.update   :as up]
            [com.yetanalytics.flint-jena.expr]
            [com.yetanalytics.flint-jena.modifier]
            [com.yetanalytics.flint-jena.path]
            [com.yetanalytics.flint-jena.select]
            [com.yetanalytics.flint-jena.triple]
            [com.yetanalytics.flint-jena.values]
            [com.yetanalytics.flint-jena.where]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flint -> AST conformer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- conform-sparql-err-map
  [error-kw error-loc-kws sparql]
  {:kind    error-kw
   :input   sparql
   :clauses error-loc-kws})

(defn- conform-sparql
  "Convert from Flint concrete syntax to the internal abstract syntax tree.
   Has the exact same functionality as in vanilla Flint."
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Need this since kwarg destructuring doesn't correctly apply default args
(def default-opts
  "The default options for flint-jena API functions."
  {:spec-ed?      false
   :iri->datatype ax/xsd-datatype-map
   :aggregate-fns #{}})

(defn- merge-opts
  "Construct the final opts map, overriding defaults and adding internal opts."
  [opts prologue]
  (-> (merge default-opts opts)
      (merge {:prologue       prologue
              :blank-node-map (ax/blank-node-map)
              :blank-var-map  (ax/blank-var-map)})))

(defn- conj-prologue
  "Reduce function where `prologue-coll` is the accumulator, and `update-ast`
   is the next AST from which to extract the prologue from."
  [opts prologue-coll update-ast]
  (if-some [prev-pro (last prologue-coll)]
    (let [next-pro  (pro/create-prologue opts update-ast)
          next-pro* (pro/merge-prologues prev-pro next-pro)]
      (conj prologue-coll next-pro*))
    (let [next-pro (pro/create-prologue opts update-ast)]
      (conj prologue-coll next-pro))))

(defn- ->update-map
  "Convert the args into a map arg accepted by `update/create-updates`."
  [prologue opts update-ast]
  {:prologue   prologue
   :opts       opts
   :update-ast update-ast})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Putting it all together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_{:clj-kondo/ignore [:unused-binding]}
(defn create-query
  [query & {:keys [spec-ed? iri->datatype aggregate-fns]
            :or   {spec-ed?      false
                   iri->datatype ax/xsd-datatype-map
                   aggregate-fns #{}}
            :as   opts}]
  (let [query-ast (conform-query spec-ed? query)
        prologue  (pro/create-prologue opts query-ast)
        opts*     (merge-opts opts prologue)]
    (qu/create-query prologue opts* query-ast)))

#_{:clj-kondo/ignore [:unused-binding]}
(defn create-updates
  [updates & {:keys [spec-ed? iri->datatype aggregate-fns]
              :or   {spec-ed?      false
                     iri->datatype ax/xsd-datatype-map
                     aggregate-fns #{}}
              :as   opts}]
  (let [update-asts (map #(conform-update spec-ed? %) updates)
        prologues   (reduce (partial conj-prologue opts) [] update-asts)
        opts-coll   (map (partial merge-opts opts) prologues)
        update-coll (mapv ->update-map prologues opts-coll update-asts)]
    (up/create-updates update-coll)))

#_{:clj-kondo/ignore [:unused-binding]}
(defn create-update
  [update & {:keys [spec-ed? iri->datatype aggregate-fns]
             :or   {spec-ed?      false
                    iri->datatype ax/xsd-datatype-map
                    aggregate-fns #{}}}]
  (create-updates [update]
                  :spec-ed?      spec-ed?
                  :iri-datatype  iri->datatype
                  :aggregate-fns aggregate-fns))
