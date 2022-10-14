(ns com.yetanalytics.flint-jena.query
  (:require [com.yetanalytics.flint-jena.ast :as ast]
            [com.yetanalytics.flint-jena.expr]
            [com.yetanalytics.flint-jena.modifier :as mod]
            [com.yetanalytics.flint-jena.select   :as sel]
            [com.yetanalytics.flint-jena.values   :as values]
            [com.yetanalytics.flint-jena.where    :as where])
  (:import [org.apache.jena.sparql.core BasicPattern TriplePath]
           [org.apache.jena.graph Node]
           [org.apache.jena.query Query]
           [org.apache.jena.sparql.syntax
            ElementPathBlock
            Template
            TripleCollectorBGP]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multimethods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti query-add! ast/ast-node-dispatch)

(defmethod query-add! :default [_ _ _] nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph URIs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :from [_ [_ iri-node]] iri-node)
(defmethod ast/ast-node->jena :from-named [_ [_ iri-nodes]] iri-nodes)

(defmethod query-add! :from
  [^Query query opts from-ast]
  (let [^Node iri-node (ast/ast->jena opts from-ast)]
    (.addGraphURI query (.getURI iri-node))))

(defmethod query-add! :from-named
  [^Query query opts from-named-ast]
  (let [iri-nodes (ast/ast->jena opts from-named-ast)]
    (run! (fn [^Node iri-node]
            (.addNamedGraphURI query (.getURI iri-node)))
          iri-nodes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SELECT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :select/expr-as-var [_opts [_ expr-as-var]]
  expr-as-var)

(defmethod ast/ast-node->jena :select/var-or-exprs [_opts [_ var-or-exprs]]
  var-or-exprs)

(defmethod ast/ast-node->jena :select [_ [_ select]]
  select)
(defmethod ast/ast-node->jena :select-distinct [_ [_ select-distinct]]
  select-distinct)
(defmethod ast/ast-node->jena :select-reduced [_ [_ select-reduced]]
  select-reduced)

(defmethod query-add! :select
  [^Query query opts select-ast]
  (sel/add-select! query opts select-ast))

(defmethod query-add! :select-distinct
  [^Query query opts select-ast]
  (sel/add-select-distinct! query opts select-ast))

(defmethod query-add! :select-reduced
  [^Query query opts select-ast]
  (sel/add-select-reduced! query opts select-ast))

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

(defmethod ast/ast-node->jena :construct [_ [_ triple-elements]]
  (Template. (elements->nopath-triples triple-elements)))

(defmethod query-add! :construct
  [^Query query opts construct-ast]
  (let [construct-temp (ast/ast->jena opts construct-ast)]
    (.setConstructTemplate query construct-temp)))

;; DESCRIBE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :describe/vars-or-iris [_ [_ var-or-iris]]
  var-or-iris)

(defmethod ast/ast-node->jena :describe [_ [_ describe]]
  describe)

(defmethod query-add! :describe
  [^Query query opts describe-ast]
  (let [describes (ast/ast->jena opts describe-ast)]
    (if (= :* describes)
      (.setQueryResultStar query true)
      (run! (fn [^Node dn] (.addDescribeNode query dn)) describes))))

;; ASK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :ask [_ [_ ask]] ask)

;; Nothing special to set for ASK clause, so this is a no-op
(defmethod query-add! :ask [_query _opts _ask-node] nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Query elements and modifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod query-add! :group-by
  [query opts group-by-ast]
  (mod/add-group-bys! query opts group-by-ast))

(defmethod query-add! :order-by
  [query opts order-by-ast]
  (mod/add-order-bys! query opts order-by-ast))

(defmethod query-add! :having
  [query opts having-ast]
  (mod/add-having! query opts having-ast))

(defmethod query-add! :limit
  [query opts limit-ast]
  (mod/add-limit! query opts limit-ast))

(defmethod query-add! :offset
  [query opts offset-ast]
  (mod/add-offset! query opts offset-ast))

(defmethod query-add! :values
  [query opts values-ast]
  (values/add-values! query opts values-ast))

(defmethod query-add! :where
  [query opts where-ast]
  (where/add-where! query opts where-ast))

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
  (run! (fn [ast-node] (query-add! query opts ast-node)) query-ast))

(defn create-query
  ([opts [query-type query-ast]]
   (doto (Query.)
     (set-query-type! query-type)
     (add-query-clauses! opts query-ast)))
  ([prologue opts [query-type query-ast]]
   (doto (Query. prologue)
     (set-query-type! query-type)
     (add-query-clauses! opts query-ast))))
