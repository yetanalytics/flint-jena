(ns com.yetanalytics.test-support
  (:import [java.util
            List]
           [org.apache.jena.graph
            Node
            Triple]
           [org.apache.jena.sparql.core
            Quad
            Var]
           [org.apache.jena.sparql.expr
            Expr]
           [org.apache.jena.sparql.syntax
            Element
            ElementAssign
            ElementBind
            ElementData
            ElementDataset
            ElementExists
            ElementFilter
            ElementGroup
            ElementMinus
            ElementNamedGraph
            ElementNotExists
            ElementOptional
            ElementPathBlock
            ElementService
            ElementSubQuery
            ElementTriplesBlock
            ElementUnion]
           [org.apache.jena.sparql.syntax.syntaxtransform
            ElementTransform
            QueryTransformOps]
           [org.apache.jena.query Query]))

(defn group-element-path-blocks
  [sub-elements]
  (let [elem-groups (partition-by type sub-elements)
        group-elem  (ElementGroup.)]
    (->> elem-groups
         (mapcat (fn [elem-group]
                   (if (instance? ElementPathBlock (first elem-group))
                     (let [acc (ElementPathBlock.)]
                       (dorun (for [pblock elem-group
                                    triple (iterator-seq
                                            (.patternElts ^ElementPathBlock pblock))]
                                (.addTriplePath acc triple)))
                       [acc])
                     elem-group)))
         (run! (fn [element]
                 (.addElement group-elem element))))
    group-elem))

#_{:clj-kondo/ignore [:unused-binding]}
(def ^ElementTransform element-transformer
  (reify ElementTransform
    (^Triple transform [this ^Triple triple]
      triple)
    (^Quad transform [this ^Quad quad]
      quad)
    (^Element transform [this ^ElementAssign el ^Var v ^Expr expr]
      (ElementAssign. v expr))
    (^Element transform [this ^ElementBind el ^Var v ^Expr expr]
      (ElementBind. v expr))
    (^Element transform [this ^ElementData el]
      el)
    (^Element transform [this ^ElementDataset el ^Element sub-elt]
      el)
    (^Element transform [this ^ElementExists el ^Element sub-elt]
      (ElementExists. sub-elt))
    (^Element transform [this ^ElementFilter el ^Expr expr]
      (ElementFilter. expr))
    (^Element transform [this ^ElementGroup el ^List members]
      (group-element-path-blocks members))
    (^Element transform [this ^ElementMinus el ^Element elt-rhs]
      (ElementMinus. elt-rhs))
    (^Element transform [this ^ElementNamedGraph el ^Node g-node ^Element sub-elt]
      (ElementNamedGraph. g-node sub-elt))
    (^Element transform [this ^ElementNotExists el ^Element sub-elt]
      (ElementNotExists. sub-elt))
    (^Element transform [this ^ElementOptional el ^Element op-elt]
      (ElementOptional. op-elt))
    (^Element transform [this ^ElementPathBlock el]
      el)
    (^Element transform [this ^ElementService el ^Node s-node ^Element sub-elt]
      (ElementService. s-node sub-elt (.getSilent el)))
    (^Element transform [this ^ElementSubQuery el ^Query query]
      (ElementSubQuery. query))
    (^Element transform [this ^ElementTriplesBlock el]
      el)
    (^Element transform [this ^ElementUnion el ^List elements]
      (let [union (ElementUnion.)]
        (run! #(.addElement union %) elements)
        union))))

(defn transform-query
  "Perform the following transformations on `query`:
   - Group ElementPathBlock triples by subject and predicate."
  [^Query query]
  (QueryTransformOps/transform query element-transformer))
