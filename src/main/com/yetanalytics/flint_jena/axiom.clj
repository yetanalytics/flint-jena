(ns com.yetanalytics.flint-jena.axiom
  (:require [clojure.walk                          :as w]
            [com.yetanalytics.flint-jena.ast       :as ast]
            [com.yetanalytics.flint.axiom.protocol :as p]
            [com.yetanalytics.flint.axiom.impl])
  (:import [org.apache.jena.atlas.lib EscapeStr]
           [org.apache.jena.datatypes RDFDatatype]
           [org.apache.jena.datatypes.xsd XSDDatatype]
           [org.apache.jena.graph NodeFactory]
           [org.apache.jena.irix IRIx]
           [org.apache.jena.sparql.core Prologue Var]
           [org.apache.jena.sparql.graph NodeConst]
           [org.apache.jena.sparql.lang LabelToNodeMap]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blank Nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The need for all these blank node utilities is that under the hood, blank
;; nodes in Jena queries are represented by Var objects, not blank Node objects.
;; The exception is in CONSTRUCT templates, hence utilities to convert Vars into
;; blank Nodes.

;; In addition, we need these node maps to ensure that a) our compiled queries
;; match those created by Jena parsers, and b) ensure that equality and hash ops
;; can work properly on queries.

(defn blank-node-map []
  (LabelToNodeMap/createBNodeMap))

(defn blank-var-map []
  (LabelToNodeMap/createVarMap))

(defmulti annotate-raw-bnode
  "Annotate whether a blank node should be compiled as a Node_Blank object
   (i.e. \"raw bnode\") or as a Var object. The annotation becomes a third
   entry in the AST vector."
  ast/ast-node-dispatch)

(defmethod annotate-raw-bnode :default [ast-node] ast-node)

(defmethod annotate-raw-bnode :ax/bnode [[bnode-type bnode-str]]
  [bnode-type bnode-str true])

(defn annotate-raw-bnodes
  "Walk the `ast` tree and mark all blank nodes as raw."
  [ast]
  (w/postwalk annotate-raw-bnode ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literal Datatypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See: https://github.com/apache/jena/blob/main/jena-core/src/main/java/org/apache/jena/datatypes/xsd/XSDDatatype.java

(def xsd-datatypes
  "A list of all static `RDFDatatype` fields defined in `XSDDatatype`."
  [;; Numerics
   XSDDatatype/XSDdecimal
   XSDDatatype/XSDinteger
   XSDDatatype/XSDnonPositiveInteger
   XSDDatatype/XSDnonNegativeInteger
   XSDDatatype/XSDpositiveInteger
   XSDDatatype/XSDnegativeInteger
   ;; Numeric types
   XSDDatatype/XSDdouble
   XSDDatatype/XSDfloat
   XSDDatatype/XSDbyte
   XSDDatatype/XSDshort
   XSDDatatype/XSDint
   XSDDatatype/XSDlong
   XSDDatatype/XSDunsignedByte
   XSDDatatype/XSDunsignedShort
   XSDDatatype/XSDunsignedInt
   XSDDatatype/XSDunsignedLong
   ;; Booleans and hexes
   XSDDatatype/XSDboolean
   XSDDatatype/XSDbase64Binary
   XSDDatatype/XSDhexBinary
   ;; Date and time
   XSDDatatype/XSDdate
   XSDDatatype/XSDtime
   XSDDatatype/XSDdateTime
   XSDDatatype/XSDdateTimeStamp
   XSDDatatype/XSDduration
   XSDDatatype/XSDyearMonthDuration
   XSDDatatype/XSDdayTimeDuration
   XSDDatatype/XSDgYearMonth
   XSDDatatype/XSDgMonthDay
   XSDDatatype/XSDgMonth
   XSDDatatype/XSDgDay
   XSDDatatype/XSDgYear
   ;; Strings
   XSDDatatype/XSDnormalizedString
   XSDDatatype/XSDstring
   XSDDatatype/XSDanyURI
   ;; Misc
   XSDDatatype/XSDtoken
   XSDDatatype/XSDName
   XSDDatatype/XSDlanguage
   XSDDatatype/XSDQName
   XSDDatatype/XSDNMTOKEN
   XSDDatatype/XSDID
   XSDDatatype/XSDENTITY
   XSDDatatype/XSDNCName
   XSDDatatype/XSDNOTATION
   XSDDatatype/XSDIDREF])

(defn- register-datatype
  ([datatype-m ^RDFDatatype datatype]
   (let [datatype-iri (->> datatype .getURI (format "<%s>"))]
     (assoc datatype-m datatype-iri datatype)))
  ([datatype-m ^RDFDatatype datatype mapped-datatype]
   (let [datatype-iri (->> datatype .getURI (format "<%s>"))]
     (assoc datatype-m datatype-iri mapped-datatype))))

(def xsd-datatype-map*
  "A mapping from all XSD datatype IRIs to their respective `RDFDatatype`
   instances. Unlike a `xsd-datatype-map`, all IRIs are mapped to their
   datatypes without overrides."
  (reduce register-datatype {} xsd-datatypes))

(def xsd-datatype-map
  "A mapping from all XSD datatype IRIs to their respective `RDFDatatype`
   instances. All numeric type IRIs are mapped to either `XSDinteger` or
   `XSDdecimal`, to match how the Jena parser assigns numeric literal types."
  (let [register-integers (partial reduce
                                   #(register-datatype %1
                                                       %2
                                                       XSDDatatype/XSDinteger))
        register-decimals (partial reduce
                                   #(register-datatype %1
                                                       %2
                                                       XSDDatatype/XSDdecimal))]
    (-> xsd-datatype-map*
        (register-integers [XSDDatatype/XSDbyte
                            XSDDatatype/XSDshort
                            XSDDatatype/XSDint
                            XSDDatatype/XSDlong
                            XSDDatatype/XSDunsignedByte
                            XSDDatatype/XSDunsignedShort
                            XSDDatatype/XSDunsignedInt
                            XSDDatatype/XSDunsignedLong])
        (register-decimals [XSDDatatype/XSDfloat
                            XSDDatatype/XSDdouble]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exceptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- throw-prefix-not-found
  [prologue iri]
  (throw (ex-info
          (format "Prefixed IRI '%s' does not have prefix in prologue." iri)
          {:kind     ::prefix-not-found
           :iri      iri
           :prologue prologue})))

(defn- throw-datatype-not-found
  [iri-datatype iri]
  (throw (ex-info (format "Datatype cannot be retrieved for IRI '%s'." iri)
                  {:kind      ::datatype-not-found
                   :iri       iri
                   :datatypes iri-datatype})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node Axioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :ax/rdf-type [_ _]
  NodeConst/nodeRDFType)

(defmethod ast/ast-node->jena :ax/iri
  [{:keys [^Prologue prologue]} [_ iri]]
  (let [^String iri-str* (p/-format-iri iri)
        ^String iri-str  (.substring iri-str* 1 (-> iri-str* .length dec))]
    (if-some [^IRIx base (some-> prologue .getBase)]
      (NodeFactory/createURI (.toString (.resolve base iri-str)))
      (NodeFactory/createURI iri-str))))

(defmethod ast/ast-node->jena :ax/prefix-iri
  [{:keys [^Prologue prologue]} [_ prefix-iri]]
  (let [^String iri (p/-format-prefix-iri prefix-iri)]
    (if-some [expanded-iri (.expandPrefixedName prologue iri)]
      (NodeFactory/createURI expanded-iri)
      (throw-prefix-not-found prologue iri))))

(defmethod ast/ast-node->jena :ax/var
  [_ [_ variable]]
  (let [^String var-str (p/-format-variable variable)]
    (-> var-str (.substring 1) Var/alloc)))

(defmethod ast/ast-node->jena :ax/bnode
  [{:keys [blank-var-map blank-node-map]} [_ bnode raw-bnode?]]
  (let [^String bnode-string    (p/-format-bnode bnode)
        ^LabelToNodeMap bnode-m (if raw-bnode? blank-node-map blank-var-map)]
    (if (= "[]" bnode-string)
      (-> bnode-m .allocNode)
      (-> bnode-m (.asNode (.substring bnode-string 1))))))

(defn- iri->dt
  [iri->datatype dt-iri]
  (try (iri->datatype dt-iri)
       (catch Exception _
         (throw-datatype-not-found iri->datatype dt-iri))))

(defmethod ast/ast-node->jena :ax/literal
  [{:keys [iri->datatype]} [_ literal]]
  (let [^String strval (-> literal
                           p/-format-literal-strval
                           (EscapeStr/unescape \\ false))
        ?literal-ltag  (-> literal
                           p/-format-literal-lang-tag)
        ?literal-dtype (some->> literal
                                p/-format-literal-url
                                (iri->dt iri->datatype))]
    (cond
      (some? ?literal-ltag)
      (NodeFactory/createLiteral strval ^String ?literal-ltag)
      (some? ?literal-dtype)
      (NodeFactory/createLiteral strval ^RDFDatatype ?literal-dtype)
      :else
      (NodeFactory/createLiteral strval))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-node Axioms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These axioms are generally used in specific places in the query/update.

(defmethod ast/ast-node->jena :ax/prefix [_ [_ prefix]]
  (p/-format-prefix prefix))

(defmethod ast/ast-node->jena :ax/numeric [_ [_ num]] num)

(defmethod ast/ast-node->jena :ax/wildcard [_ _] :*)

