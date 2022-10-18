(ns com.yetanalytics.flint-jena.query
  (:require [com.yetanalytics.flint-jena.ast :as ast]
            [com.yetanalytics.flint-jena.expr]
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
;; Multimethods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti query-add! ast/ast-node-dispatch)

(defmethod query-add! :default [_ _] nil)

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

(defn- elements->nopath-triples ^BasicPattern [triple-elements]
  (let [acc (TripleCollectorBGP.)]
    (dorun
     (for [t-elem triple-elements
           triple (->> (.patternElts ^ElementPathBlock t-elem)
                       iterator-seq
                       (map #(.asTriple ^TriplePath %)))]
       (.addTriple acc triple)))
    (.getBGP acc)))

(defmethod ast/ast-node->jena :construct [_ [kw triple-elements]]
  [kw (Template. (elements->nopath-triples triple-elements))])

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
(defmethod query-add! :ask [_query _ask-node] nil)

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

;; In case the CONSTRUCT clause is empty
;; This should always be called after the CONSTRUCT since the AST is sorted
(defn- add-where-as-construct!
  [^Query query where-ast]
  (when (and (.isConstructType query)
             (-> query .getConstructTemplate .getTriples not-empty nil?))
    (try (->> (.getElements ^ElementGroup where-ast)
              elements->nopath-triples
              Template.
              (.setConstructTemplate query))
         (catch Exception _
           (throw (IllegalArgumentException.
                   "CONSTRUCT WHERE must only consist of Basic Graph Patterns!"))))))

(defmethod query-add! :where
  [query [_ where-ast]]
  (where/add-where! query where-ast)
  (add-where-as-construct! query where-ast))

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
  [prologue opts [query-type query-ast]]
  (doto (Query.)
    (pro/add-prologue! prologue)
    (set-query-type! query-type)
    (add-query-clauses! opts query-ast)))
