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
            UpdateDeleteInsert
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

(defmethod ast/ast-node->jena :load [_ [_ ^Node iri-node]]
  (.getURI iri-node))
(defmethod ast/ast-node->jena :load-silent [_ [_ ^Node iri-node]]
  (.getURI iri-node))

(defmethod ast/ast-node->jena :into [_ [_ ^Target target]]
  (.getGraph target))

(defmethod ast/ast-node->jena :clear [_ [_ target]] target)
(defmethod ast/ast-node->jena :clear-silent [_ [_ target]] target)

(defmethod ast/ast-node->jena :drop [_ [_ target]] target)
(defmethod ast/ast-node->jena :drop-silent [_ [_ target]] target)

(defmethod ast/ast-node->jena :create [_ [_ ^Target target]]
  (.getGraph target))
(defmethod ast/ast-node->jena :create-silent [_ [_ ^Target target]]
  (.getGraph target))

(defmethod ast/ast-node->jena :add [_ [_ src-target]] src-target)
(defmethod ast/ast-node->jena :add-silent [_ [_ src-target]] src-target)

(defmethod ast/ast-node->jena :add [_ [_ src-target]] src-target)
(defmethod ast/ast-node->jena :add-silent [_ [_ src-target]] src-target)

(defmethod ast/ast-node->jena :move [_ [_ src-target]] src-target)
(defmethod ast/ast-node->jena :move-silent [_ [_ src-target]] src-target)

(defmethod ast/ast-node->jena :copy [_ [_ src-target]] src-target)
(defmethod ast/ast-node->jena :copy-silent [_ [_ src-target]] src-target)

(defmethod ast/ast-node->jena :to [_ [_ dest-target]] dest-target)

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

(defmethod ast/ast-node->jena :insert-data [_opts [_ elements]]
  (doto (QuadDataAcc.)
    (add-quad-elements! elements)))

(defmethod ast/ast-node->jena :delete-data [_opts [_ elements]]
  (doto (QuadDataAcc.)
    (add-quad-elements! elements)))

(defmethod ast/ast-node->jena :delete-where [_opts [_ elements]]
  (doto (QuadAcc.)
    (add-quad-elements! elements)))

(defmethod ast/ast-node->jena :delete [_opts [_ elements]]
  (let [quad-acc (doto (QuadAcc.)
                   (add-quad-elements! elements))]
    (.getQuads quad-acc)))

(defmethod ast/ast-node->jena :insert [_opts [_ elements]]
  (let [quad-acc (doto (QuadAcc.)
                   (add-quad-elements! elements))]
    (.getQuads quad-acc)))

;; Named Graph IRIs

(defmethod ast/ast-node->jena :update/iri [_opts [_ graph-node]]
  graph-node)

(defmethod ast/ast-node->jena :update/named-iri [_opts [_ [_ graph-node]]]
  graph-node)

(defmethod ast/ast-node->jena :using [_opts [_ graph-node]]
  graph-node)

(defmethod ast/ast-node->jena :with [_opts [_ ^Node graph-node]]
  (.getURI graph-node))

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
  (let [update (UpdateDeleteInsert.)
        d-acc  (.getDeleteAcc update)
        i-acc  (.getInsertAcc update)]
    (when (not-empty delete)
      (run! (fn [del] (.addQuad d-acc del)) delete)
      (.setHasDeleteClause update true))
    (when (not-empty insert)
      (run! (fn [ins] (.addQuad i-acc ins)) insert)
      (.setHasInsertClause update true))
    (when (some? where)
      (.setElement update where))
    (when (some? using)
      (.addUsing update using))
    (when (some? with)
      (.setWithIRI update with))
    update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Putting it all together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- update->jena* [opts [update-type update-ast]]
  (let [update-map (reduce (fn [m [ast-k _ :as ast]]
                             (assoc m ast-k (ast/ast->jena opts ast)))
                           {}
                           update-ast)]
    (new-update update-type update-map)))

(defn create-updates
  [prologue opts update-asts]
  (let [updates      (mapv #(update->jena* opts %) update-asts)
        add-updates! (fn [^UpdateRequest update-req updates]
                       (run! (fn [^Update up] (.add update-req up)) updates))]
    (doto (UpdateRequest.)
      (pro/add-prologue! prologue)
      (add-updates! updates))))
