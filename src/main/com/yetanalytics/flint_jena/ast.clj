(ns com.yetanalytics.flint-jena.ast
  (:require [clojure.walk :as w]
            [com.yetanalytics.flint.util :as u]))

(defn ast-node-dispatch
  "Multi-method dispatch based on `ast-node`'s keyword."
  ([ast-node]
   (if-some [k (u/get-keyword ast-node)] k :default))
  ([_ ast-node]
   (if-some [k (u/get-keyword ast-node)] k :default))
  ([_ _ ast-node]
   (if-some [k (u/get-keyword ast-node)] k :default)))

(defmulti ast-node->jena
  "Given the `ast-node`, return a Jena query/update builder instance.
   Applies and/or mutates `opts` whenever needed."
  {:arglists '([opts ast-node])}
  ast-node-dispatch)

(defmulti ast-node->jena-pre
  "Dispatching on the `ast-node` keyword, perform an action at the node
   **before** `ast-node->jena` is called. Typically used for side effects."
  {:arglists '([opts ast-node])}
  ast-node-dispatch)

(defmulti ast-node->jena-post
  "Dispatching on the `ast-node` keyword, perform an action at the node
   **after** `ast-node->jena` is called. Typically used for side effects."
  {:arglists '([opts ast-node])}
  ast-node-dispatch)

(defmethod ast-node->jena :default [_ ast-node]
  ast-node)

(defmethod ast-node->jena-pre :default [_ _]
  nil)

(defmethod ast-node->jena-post :default [_ _]
  nil)

(defn ast->jena
  "Given an `opts` map and a conformed Flint `ast`, convert it into a
   Jena instance or a coll thereof. Does so by performing tree traversal
   of `ast` and applying `ast-node->jena` (and the `-pre` and `-post`
   functions, when applicable) to each AST node."
  [opts ast]
  ;; Step 1: Apply the pre function to the node
  (ast-node->jena-pre opts ast)
  (w/walk
   ;; Step 2: Map over the node's children and perform post-order traversal
   (partial ast->jena opts)
   (fn [ast-node]
     ;; Step 3: Apply ast-node->jena to the node
     (let [res (ast-node->jena opts ast-node)]
       ;; Step 4: Apply the post function to the node
       (ast-node->jena-post opts ast-node)
       ;; Step 5: Return the result
       res))
   ast))
