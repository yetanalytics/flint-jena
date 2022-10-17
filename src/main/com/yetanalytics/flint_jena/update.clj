(ns com.yetanalytics.flint-jena.update
  (:require [com.yetanalytics.flint-jena.ast      :as ast]
            [com.yetanalytics.flint-jena.prologue :as pro])
  (:import [org.apache.jena.graph Node Triple]
           [org.apache.jena.sparql.core BasicPattern Quad QuadPattern TriplePath] 
           [org.apache.jena.sparql.syntax ElementPathBlock ElementTriplesBlock]
           [org.apache.jena.update Update UpdateRequest]
           [org.apache.jena.sparql.modify.request
            ;; Graph management
            UpdateAdd
            UpdateClear
            UpdateCopy
            UpdateCreate
            UpdateDrop
            UpdateLoad
            UpdateMove
            ;; Graph update
            UpdateDataInsert
            UpdateDataDelete
            UpdateDeleteWhere
            UpdateModify
            ;; Miscellaneous
            QuadAcc
            QuadDataAcc
            Target]))

(defmulti new-update
  (fn [update-type _] update-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :update/default [_ _] Target/DEFAULT)
(defmethod ast/ast-node->jena :update/named [_ _] Target/NAMED)
(defmethod ast/ast-node->jena :update/all [_ _] Target/ALL)

(defmethod ast/ast-node->jena :update/graph-notag [_ [_ graph-node]]
  (Target/create ^Node graph-node))
(defmethod ast/ast-node->jena :update/graph [_ [_ [_graph-k graph-node]]]
  (Target/create ^Node graph-node))

(defmethod ast/ast-node->jena :load [_ [kw ^Node iri-node]]
  [kw (.getURI iri-node)])
(defmethod ast/ast-node->jena :load-silent [_ [kw ^Node iri-node]]
  [kw (.getURI iri-node)])

(defmethod ast/ast-node->jena :into [_ [kw ^Target target]]
  [kw (.getGraph target)])

(defmethod ast/ast-node->jena :clear [_ clear-target] clear-target)
(defmethod ast/ast-node->jena :clear-silent [_ clear-target] clear-target)

(defmethod ast/ast-node->jena :drop [_ drop-target] drop-target)
(defmethod ast/ast-node->jena :drop-silent [_ drop-target] drop-target)

(defmethod ast/ast-node->jena :create [_ [kw ^Target target]]
  [kw (.getGraph target)])
(defmethod ast/ast-node->jena :create-silent [_ [kw ^Target target]]
  [kw (.getGraph target)])

(defmethod ast/ast-node->jena :add [_ src-target] src-target)
(defmethod ast/ast-node->jena :add-silent [_ src-target] src-target)

(defmethod ast/ast-node->jena :move [_ src-target] src-target)
(defmethod ast/ast-node->jena :move-silent [_ src-target] src-target)

(defmethod ast/ast-node->jena :copy [_ src-target] src-target)
(defmethod ast/ast-node->jena :copy-silent [_ src-target] src-target)

(defmethod ast/ast-node->jena :to [_ dest-target] dest-target)

(defmethod new-update :update/load
  [_ {^String ld :load
      ^String ls :load-silent
      ^Node ldin :into}]
  (UpdateLoad. (or ld ls) ldin (some? ls)))

(defmethod new-update :update/clear
  [_ {^Target cl :clear
      ^Target cs :clear-silent}]
  (UpdateClear. (or cl cs) (some? cs)))

(defmethod new-update :update/drop
  [_ {^Target dp :drop
      ^Target ds :drop-silent}]
  (UpdateDrop. (or dp ds) (some? ds)))

(defmethod new-update :update/create
  [_ {^Node cr :create
      ^Node cs :create-silent}]
  (UpdateCreate. (or cr cs) (some? cs)))

(defmethod new-update :update/add
  [_ {ad :add
      as :add-silent
      to :to}]
  (UpdateAdd. (or ad as) to (some? as)))

(defmethod new-update :update/move
  [_ {mv :move
      ms :move-silent
      to :to}]
  (UpdateMove. (or mv ms) to (some? ms)))

(defmethod new-update :update/copy
  [_ {cp :copy
      cs :copy-silent
      to :to}]
  (UpdateCopy. (or cp cs) to (some? cs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Graph Update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- triple-element->bgp ^BasicPattern [^ElementPathBlock triple-element]
  (let [triples (->> triple-element
                     .patternElts
                     iterator-seq
                     (mapv #(.asTriple ^TriplePath %)))]
    (BasicPattern/wrap triples)))

(defn- triple-elements->bgp ^BasicPattern [triple-elements]
  (let [triple-bgp (BasicPattern.)
        sub-bgps   (map triple-element->bgp triple-elements)]
    (run! (fn [sub-bgp] (.addAll triple-bgp sub-bgp)) sub-bgps)
    triple-bgp))

(defn- triples->quads
  (^QuadPattern [triples]
   (triples->quads triples Quad/tripleInQuad))
  (^QuadPattern [^BasicPattern triples graph-node]
   (let [quads   (->> triples
                      .iterator
                      iterator-seq
                      (map (fn [triple] (Quad. graph-node triple))))
         pattern (QuadPattern.)]
     (dorun (map-indexed (fn [idx quad] (.add pattern idx quad))
                         quads))
     pattern)))

(defprotocol QuadBlock
  (-add-quads! [this quad-acc]))

(extend-protocol QuadBlock
  ElementPathBlock
  (-add-quads! [paths-element ^QuadAcc quad-acc]
    (dorun (->> paths-element
                .patternElts
                iterator-seq
                (map (fn [^TriplePath triple-path]
                       (.asTriple triple-path)))
                (map (fn [^Triple triple]
                       (.addTriple quad-acc triple))))))

  ElementTriplesBlock
  (-add-quads! [triples-element ^QuadAcc quad-acc]
    (dorun (->> triples-element
                .patternElts
                iterator-seq
                (map (fn [^Triple triple]
                       (.addTriple quad-acc triple))))))

  BasicPattern
  (-add-quads! [triples ^QuadAcc quad-acc]
    (dorun (->> triples
                .iterator
                iterator-seq
                (map (fn [^Triple triple]
                       (.addTriple quad-acc triple))))))

  QuadPattern
  (-add-quads! [quads ^QuadAcc quad-acc]
    (dorun (->> quads
                .iterator
                iterator-seq
                (map (fn [^Quad quad]
                       (.addQuad quad-acc quad)))))))

(defmethod ast/ast-node->jena :triple/quad-triples
  [_opts [_ triple-elements]]
  (triple-elements->bgp triple-elements))

(defmethod ast/ast-node->jena :triple/quads
  [_opts [_ [graph-node triple-bgp]]]
  (triples->quads triple-bgp graph-node))

(defn- add-quad-elements! [quad-acc quad-elements]
  (run! (fn [quad-element] (-add-quads! quad-element quad-acc))
        quad-elements))

;; Quad Patterns

(defmethod ast/ast-node->jena :insert-data [_opts [kw elements]]
  [kw (doto (QuadDataAcc.)
        (add-quad-elements! elements))])

(defmethod ast/ast-node->jena :delete-data [_opts [kw elements]]
  [kw (doto (QuadDataAcc.)
        (add-quad-elements! elements))])

(defmethod ast/ast-node->jena :delete-where [_opts [kw elements]]
  [kw (doto (QuadAcc.)
        (add-quad-elements! elements))])

(defmethod ast/ast-node->jena :delete [_opts [kw elements]]
  [kw (let [quad-acc (doto (QuadAcc.)
                       (add-quad-elements! elements))]
        (.getQuads quad-acc))])

(defmethod ast/ast-node->jena :insert [_opts [kw elements]]
  [kw (let [quad-acc (doto (QuadAcc.)
                       (add-quad-elements! elements))]
        (.getQuads quad-acc))])

;; Named Graph IRIs

(defmethod ast/ast-node->jena :update/iri [_opts [_ graph-node]]
  graph-node)

(defmethod ast/ast-node->jena :update/named-iri [_opts [_ graph-node]]
  graph-node)

(defmethod ast/ast-node->jena :using [_opts using-graph]
  using-graph)

(defmethod ast/ast-node->jena :with [_opts with-graph]
  with-graph)

;; Update Creation

(defmethod new-update :update/insert-data
  [_ {:keys [insert-data]}]
  (UpdateDataInsert. insert-data))

(defmethod new-update :update/delete-data
  [_ {:keys [delete-data]}]
  (UpdateDataDelete. delete-data))

(defmethod new-update :update/delete-where
  [_ {:keys [delete-where]}]
  (UpdateDeleteWhere. delete-where))

(defmethod new-update :update/modify
  [_ {:keys [delete insert where with using]}]
  (let [update-obj (UpdateModify.)
        delete-acc (.getDeleteAcc update-obj)
        insert-acc (.getInsertAcc update-obj)]
    (when (not-empty delete)
      (run! (fn [del] (.addQuad delete-acc del)) delete)
      (.setHasDeleteClause update-obj true))
    (when (not-empty insert)
      (run! (fn [ins] (.addQuad insert-acc ins)) insert)
      (.setHasInsertClause update-obj true))
    (when (some? where)
      (.setElement update-obj where))
    (when (some? with)
      (.setWithIRI update-obj with))
    (when (some? using)
      (if (vector? using)
        (.addUsingNamed update-obj (second using))
        (.addUsing update-obj using)))
    update-obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Putting it all together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- update->jena* [opts [update-type update-ast]]
  (->> update-ast
       (ast/ast->jena opts)
       (filter (fn [[k _]] (not (#{:base :prefixes} k))))
       (into {})
       (new-update update-type)))

(defn create-updates
  [prologue opts update-asts]
  (let [updates      (mapv #(update->jena* opts %) update-asts)
        add-updates! (fn [^UpdateRequest update-req updates]
                       (run! (fn [^Update up] (.add update-req up)) updates))]
    (doto (UpdateRequest.)
      (pro/add-prologue! prologue)
      (add-updates! updates))))
