(ns com.yetanalytics.flint-jena.query
  (:require [com.yetanalytics.flint-jena.ast      :as ast]
            [com.yetanalytics.flint-jena.axiom    :as ax]
            [com.yetanalytics.flint-jena.modifier :as mod]
            [com.yetanalytics.flint-jena.prologue :as pro]
            [com.yetanalytics.flint-jena.select   :as sel]
            [com.yetanalytics.flint-jena.values   :as values]
            [com.yetanalytics.flint-jena.where    :as where])
  (:import [org.apache.jena.sparql.core BasicPattern TriplePath]
           [org.apache.jena.graph Node]
           [org.apache.jena.query Query]
           [org.apache.jena.sparql.syntax
            ElementGroup
            ElementPathBlock
            Template
            TripleCollectorBGP]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multimethods and Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti query-add!
  "Add the `ast-node`, which should have been converted into a Jena object
   via `ast/ast->jena`, to `query`."
  {:arglists '([query ast-node])}
  ast/ast-node-dispatch)

(defmethod query-add! :default [_ _] nil)

(defn- get-sub-ast
  "Return the sub-ast from the `query-ast` coll by its `ast-key`."
  [query-ast ast-key]
  (some (fn [[k v]] (when (#{ast-key} k) v)) query-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph URIs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :from [_ from-iri] from-iri)
(defmethod ast/ast-node->jena :from-named [_ from-iris] from-iris)

(defmethod query-add! :from
  [^Query query [_ iri-node]]
  (.addGraphURI query (.getURI ^Node iri-node)))

(defmethod query-add! :from-named
  [^Query query [_ iri-nodes]]
  (run! (fn [iri-node]
          (.addNamedGraphURI query (.getURI ^Node iri-node)))
        iri-nodes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SELECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :select/expr-as-var [_opts [_ expr-as-var]]
  expr-as-var)

(defmethod ast/ast-node->jena :select/var-or-exprs [_opts [_ var-or-exprs]]
  var-or-exprs)

(defmethod ast/ast-node->jena :select [_ select]
  select)
(defmethod ast/ast-node->jena :select-distinct [_ select-distinct]
  select-distinct)
(defmethod ast/ast-node->jena :select-reduced [_ select-reduced]
  select-reduced)

(defmethod query-add! :select
  [^Query query [_ select-ast]]
  (sel/add-select! query select-ast))

(defmethod query-add! :select-distinct
  [^Query query [_ select-ast]]
  (sel/add-select-distinct! query select-ast))

(defmethod query-add! :select-reduced
  [^Query query [_ select-ast]]
  (sel/add-select-reduced! query select-ast))

;; CONSTRUCT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTRUCT query helpers

(defn- annotate-construct-bnodes
  "Annotate the bnodes in the CONSTRUCT clause so that they are converted
   into Jena blank Nodes intead of blank Vars."
  [query-ast]
  (mapv (fn [[k v :as ast]]
          (if (#{:construct} k)
            [k (ax/annotate-raw-bnodes v)]
            ast))
        query-ast))

(defn- elements->nopath-triples
  "Convert the coll of ElementPathBlocks `triple-elements` into a BGP."
  ^BasicPattern [triple-elements]
  (let [acc (TripleCollectorBGP.)]
    (dorun (for [t-elem triple-elements
                 triple (->> (.patternElts ^ElementPathBlock t-elem)
                             iterator-seq
                             (map #(.asTriple ^TriplePath %)))]
             (.addTriple acc triple)))
    (.getBGP acc)))

(defn- replace-construct-where
  "Replaces the `:construct` and `:where` AST nodes with a single
   `:construct-where` node."
  [query-ast]
  (->> query-ast
       (mapv (fn [[k v]]
               (cond
                 (= :construct k) nil
                 (= :where k)     [:construct-where v]
                 :else            [k v])))
       (filter some?)))

;; Multimethods

(defmethod ast/ast-node->jena :construct
  [_ [kw triple-elements]]
  [kw (->> triple-elements 
           elements->nopath-triples
           Template.)])

;; Same as for `:where`
(defmethod ast/ast-node->jena :construct-where [_ where] where)

(defmethod query-add! :construct
  [^Query query [_ construct-template]]
  (.setConstructTemplate query construct-template))

;; DESCRIBE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :describe/vars-or-iris [_ [_ var-or-iris]]
  var-or-iris)

(defmethod ast/ast-node->jena :describe [_ describe]
  describe)

(defmethod query-add! :describe
  [^Query query [_ describes]]
  (if (= :* describes)
    (.setQueryResultStar query true)
    (run! (fn [^Node dn] (.addDescribeNode query dn)) describes)))

;; ASK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :ask [_ ask] ask)

;; Nothing special to set for ASK clause, so this is a no-op
(defmethod query-add! :ask [_ _] nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query elements and modifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod query-add! :group-by
  [query [_ group-by-ast]]
  (mod/add-group-bys! query group-by-ast))

(defmethod query-add! :order-by
  [query [_ order-by-ast]]
  (mod/add-order-bys! query order-by-ast))

(defmethod query-add! :having
  [query [_ having-ast]]
  (mod/add-having! query having-ast))

(defmethod query-add! :limit
  [query [_ limit-ast]]
  (mod/add-limit! query limit-ast))

(defmethod query-add! :offset
  [query [_ offset-ast]]
  (mod/add-offset! query offset-ast))

(defmethod query-add! :values
  [query [_ values-ast]]
  (values/add-values! query values-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WHERE pattern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod query-add! :where
  [query [_ where-ast]]
  (where/add-where! query where-ast))

(defn- throw-construct-where
  [where-ast]
  (throw (ex-info "CONSTRUCT WHERE must only consist of Basic Graph Patterns!"
                  {:kind ::invalid-construct-where
                   :where-ast where-ast})))

(defn- add-construct-where!
  "Add function that should be called whenever the CONSTRUCT clause is empty.
   Adds the CONSTRUCT template based on the WHERE pattern; throws an exception
   if said pattern is not a BGP. Called in conjunction with the regular
   `add-where!`"
  [^Query query where-ast]
  (try (->> (.getElements ^ElementGroup where-ast)
            elements->nopath-triples
            Template.
            (.setConstructTemplate query))
       (catch Exception _
         (throw-construct-where where-ast))))

(defmethod query-add! :construct-where
  [query [_ where-ast]]
  (where/add-where! query where-ast)
  (add-construct-where! query where-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Putting it all together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- set-query-type!
  [^Query query query-type]
  (case query-type
    :query/select    (.setQuerySelectType query)
    :query/construct (.setQueryConstructType query)
    :query/describe  (.setQueryDescribeType query)
    :query/ask       (.setQueryAskType query)))

(defn- add-query-clauses!
  [query opts query-ast]
  (->> query-ast
       (ast/ast->jena opts)
       (filter (fn [[k _]] (not (#{:base :prefixes} k))))
       (run! (fn [ast-node] (query-add! query ast-node)))))

(defn create-query
  "Create a new Query instance with the contents of `prologue` and `query-ast`
   and its type (SELECT, CONSTRUCT, etc.) set by `query-type`."
  [prologue opts [query-type query-ast]]
  (let [construct? (= :query/construct query-type)
        query-ast* (cond-> query-ast
                     construct?
                     annotate-construct-bnodes
                     (and construct?
                          (empty? (get-sub-ast query-ast :construct)))
                     replace-construct-where)
        query (Query.)]
    (doto query
      (pro/add-prologue! prologue)
      (set-query-type! query-type)
      (add-query-clauses! (assoc opts :query query) query-ast*))))
