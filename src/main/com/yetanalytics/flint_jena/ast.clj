(ns com.yetanalytics.flint-jena.ast
  (:require [clojure.walk :as w]
            [com.yetanalytics.flint.util :as u]))

(defn ast-node-dispatch
  ([ast-node]
   (if-some [k (u/get-keyword ast-node)] k :default))
  ([_ ast-node]
   (if-some [k (u/get-keyword ast-node)] k :default))
  ([_ _ ast-node]
   (if-some [k (u/get-keyword ast-node)] k :default)))

(defmulti ast-node->jena
  "Given the AST node `x`, return a Jena query/update builder instance."
  ast-node-dispatch)

(defmethod ast-node->jena :default [_ ast-node]
  ast-node)

(defn ast->jena
  "Given an `opts` map and a conformed Flint `ast`, convert it into a
   Jena instance or a coll thereof."
  [opts ast]
  (let [ast-node->jena* (partial ast-node->jena opts)]
    (w/postwalk ast-node->jena* ast)))
