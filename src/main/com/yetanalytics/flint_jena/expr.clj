(ns com.yetanalytics.flint-jena.expr
  (:require [com.yetanalytics.flint-jena.ast :as ast])
  (:import [java.util List]
           [org.apache.jena.graph Node]
           [org.apache.jena.query Query]
           [org.apache.jena.sparql.core Prologue Var]
           [org.apache.jena.sparql.syntax Element ElementBind]
           [org.apache.jena.sparql.expr.aggregate
            Aggregator
            AggregatorFactory]
           [org.apache.jena.sparql.expr
            ;; Arithmetic and Numeric Expressions
            E_Add E_Subtract E_Multiply E_Divide
            E_GreaterThan E_GreaterThanOrEqual
            E_LessThan E_LessThanOrEqual
            E_NumAbs E_NumCeiling E_NumFloor E_NumRound
            E_Random
            ;; Logical and Predicate Expressions
            E_LogicalAnd E_LogicalOr E_LogicalNot
            E_Equals E_NotEquals E_OneOf E_NotOneOf
            E_IsBlank E_IsIRI E_IsLiteral E_IsNumeric
            E_Bound E_LangMatches E_SameTerm
            E_Exists E_NotExists
            E_Conditional
            ;; Date/Time Expressions
            E_DateTimeHours E_DateTimeMinutes E_DateTimeSeconds
            E_DateTimeYear E_DateTimeMonth E_DateTimeDay
            E_DateTimeTimezone E_DateTimeTZ
            E_Now
            ;; String Expressions
            E_Str E_StrLength E_StrDatatype E_StrLang
            E_StrUUID E_StrEncodeForURI
            E_StrConcat E_StrContains E_StrReplace E_StrSubstring
            E_StrBefore E_StrAfter
            E_StrStartsWith E_StrEndsWith
            E_StrUpperCase E_StrLowerCase
            E_Regex
            ;; Hash Expressions
            E_MD5 E_SHA1 E_SHA256 E_SHA384 E_SHA512
            ;; Misc Expressions
            E_BNode E_IRI E_UUID
            E_Coalesce
            E_Lang E_Datatype
            ;; Other Expr Classes
            E_Function
            Expr
            ExprAggregator
            ExprList
            ExprVar
            NodeValue]))

(defn- jena-expr-dispatch [_opts op _args]
  (if (symbol? op) op :custom))

(defmulti ast-node->jena-expr
  "Instantiate a new Expr object; dispatched on `op`."
  {:arglists '([opts op args])}
  jena-expr-dispatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- defexpr*
  [sym class-name arity]
  `(defmethod ast-node->jena-expr ~sym
     [~'opts ~'op ~'args]
     ~(case arity
        :zero `(new ~class-name)
        :one  `(new ~class-name (first ~'args))
        :two  `(new ~class-name (first ~'args) (second ~'args)))))

(defmacro defvariadic [sym class-name]
  `(defmethod ast-node->jena-expr ~sym
     [~'opts ~'op ~'args]
     (cond
       (< (count ~'args) 2)
       (throw (ex-info (-> "Expression '%s' cannot have less than 2 arguments!"
                           (format ~sym))
                       {:kind ::invalid-expr-arity
                        :expr ~sym
                        :args ~'args}))
       (= (count ~'args) 2)
       (new ~class-name
            (first ~'args)
            (second ~'args))
       :else ; All defvaradic ops are left-associative
       (new ~class-name
            (ast-node->jena-expr ~'opts ~'op (vec (butlast ~'args)))
            (last ~'args)))))

(defmacro defnilary
  [sym class-name]
  (defexpr* sym class-name :zero))

(defmacro defunary
  [sym class-name]
  (defexpr* sym class-name :one))

(defmacro defbinary
  [sym class-name]
  (defexpr* sym class-name :two))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regular Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nilary expressions

(defnilary 'rand    E_Random)
(defnilary 'now     E_Now)
(defnilary 'uuid    E_UUID)
(defnilary 'struuid E_StrUUID)

;; Unary expressions

(defmethod ast-node->jena-expr 'bnode
  [_ _ args]
  (if (zero? (count args))
    (E_BNode/create)
    (E_BNode/create (first args))))

(defunary 'not E_LogicalNot)

(defunary 'datatype E_Datatype)
(defunary 'lang     E_Lang)

(defunary 'str            E_Str)
(defunary 'strlen         E_StrLength)
(defunary 'ucase          E_StrUpperCase)
(defunary 'lcase          E_StrLowerCase)
(defunary 'encode-for-uri E_StrEncodeForURI)

(defunary 'bound E_Bound)

(defunary 'blank?   E_IsBlank)
(defunary 'literal? E_IsLiteral)
(defunary 'numeric? E_IsNumeric)
(defunary 'iri?     E_IsIRI)
(defunary 'uri?     E_IsIRI)

(defunary 'abs   E_NumAbs)
(defunary 'ceil  E_NumCeiling)
(defunary 'floor E_NumFloor)
(defunary 'round E_NumRound)

(defunary 'year     E_DateTimeYear)
(defunary 'month    E_DateTimeMonth)
(defunary 'day      E_DateTimeDay)
(defunary 'hours    E_DateTimeHours)
(defunary 'minutes  E_DateTimeMinutes)
(defunary 'seconds  E_DateTimeSeconds)
(defunary 'timezone E_DateTimeTimezone)
(defunary 'tz       E_DateTimeTZ)

(defunary 'md5    E_MD5)
(defunary 'sha1   E_SHA1)
(defunary 'sha256 E_SHA256)
(defunary 'sha384 E_SHA384)
(defunary 'sha512 E_SHA512)

;; Need to define 'iri and 'uri manually since they accept the base URI
;; as a constructor argument.

(defmethod ast-node->jena-expr 'iri
  [{:keys [prologue]} _ [iri-arg]]
  (E_IRI. (.getBaseURI ^Prologue prologue) iri-arg))

(defmethod ast-node->jena-expr 'uri
  [{:keys [prologue]} _ [uri-arg]]
  (E_IRI. (.getBaseURI ^Prologue prologue) uri-arg))

;; Need to define 'exists and 'not-exists manually since the E_Exists and
;; E_NotExists constructors are overloaded and require type hints.

(defmethod ast-node->jena-expr 'exists
  [_ _ [where-clause]]
  (E_Exists. ^Element where-clause))

(defmethod ast-node->jena-expr 'not-exists
  [_ _ [where-clause]]
  (E_NotExists. ^Element where-clause))

;; Binary Expressions

(defbinary 'lang-matches E_LangMatches)
(defbinary 'contains     E_StrContains)
(defbinary 'strlang      E_StrLang)
(defbinary 'strdt        E_StrDatatype)
(defbinary 'strstarts    E_StrStartsWith)
(defbinary 'strends      E_StrEndsWith)
(defbinary 'strbefore    E_StrBefore)
(defbinary 'strafter     E_StrAfter)

(defbinary 'sameterm E_SameTerm)

(defbinary '=    E_Equals)
(defbinary 'not= E_NotEquals)
(defbinary '<    E_LessThan)
(defbinary '>    E_GreaterThan)
(defbinary '<=   E_LessThanOrEqual)
(defbinary '>=   E_GreaterThanOrEqual)

;; Binary+ expressions

(defvariadic 'and E_LogicalAnd)
(defvariadic 'or  E_LogicalOr)
(defvariadic '+   E_Add)
(defvariadic '-   E_Subtract)
(defvariadic '*   E_Multiply)
(defvariadic '/   E_Divide)

(defmethod ast-node->jena-expr 'in
  [_ _ [expr & ^List expr-coll]]
  (E_OneOf. expr (ExprList. expr-coll)))

(defmethod ast-node->jena-expr 'not-in
  [_ _ [expr & ^List expr-coll]]
  (E_NotOneOf. expr (ExprList. expr-coll)))

;; 3-arity, 4-arity, and varardic expressions

(defmethod ast-node->jena-expr 'regex
  [_ _ [str-expr pat-expr ?flags-expr]]
  (E_Regex. ^Expr str-expr ^Expr pat-expr ^Expr ?flags-expr))

(defmethod ast-node->jena-expr 'substr
  [_ _ [str-expr loc-expr ?len-expr]]
  (E_StrSubstring. str-expr loc-expr ?len-expr))

(defmethod ast-node->jena-expr 'if
  [_ _ [cond-expr then-expr else-expr]]
  (E_Conditional. cond-expr then-expr else-expr))

(defmethod ast-node->jena-expr 'replace
  [_ _ [str-expr pat-expr replace-expr ?flags-expr]]
  (E_StrReplace. str-expr pat-expr replace-expr ?flags-expr))

(defmethod ast-node->jena-expr 'concat
  [_ _ ^List expr-coll]
  (E_StrConcat. (ExprList. expr-coll)))

(defmethod ast-node->jena-expr 'coalesce
  [_ _ ^List expr-coll]
  (E_Coalesce. (ExprList. expr-coll)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aggregate Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- assert-query
  "Assert that the query being construct is present in the opts map."
  [query]
  (when (nil? query)
    (throw (ex-info "Missing query in opts map!"
                    {:kind ::query-not-present}))))

(defn- assert-no-nested-aggs
  "Enforce Jena's requirement that aggregates cannot be nested. (Note that
   this is not required by the SPARQL spec, so vanilla Flint does not check
   for it.)"
  [op args]
  (when (some #(instance? ExprAggregator %) args)
    (throw (ex-info (format "Nested aggregate is not allowed in '%s'!"
                            (name op))
                    {:kind     ::nested-aggregate
                     :agg-op   (name op)
                     :agg-args args}))))

(defn- agg->expr
  "Return a new aggregate expression with a bound variable.
   
   IMPORTANT: This applies a side effect to the `query` being constructed.
   Unfortunately, Jena makes it extremely difficult to perform this operation
   in a pure functional manner, but since internally the aggregate maps are
   different fields from the query body this should be fine."
  [{:keys [^Query query]} ^Aggregator agg]
  (assert-query query)
  ;; .allocAggregate returns the new agg expr
  (.allocAggregate query agg))

(defmethod ast-node->jena-expr 'sum
  [{dist? :distinct? :or {dist? false} :as opts} op [arg :as args]]
  (assert-no-nested-aggs op args)
  (agg->expr opts (AggregatorFactory/createSum dist? arg)))

(defmethod ast-node->jena-expr 'min
  [{dist? :distinct? :or {dist? false} :as opts} op [arg :as args]]
  (assert-no-nested-aggs op args)
  (agg->expr opts (AggregatorFactory/createMin dist? arg)))

(defmethod ast-node->jena-expr 'max
  [{dist? :distinct? :or {dist? false} :as opts} op [arg :as args]]
  (assert-no-nested-aggs op args)
  (agg->expr opts (AggregatorFactory/createMax dist? arg)))

(defmethod ast-node->jena-expr 'avg
  [{dist? :distinct? :or {dist? false} :as opts} op [arg :as args]]
  (assert-no-nested-aggs op args)
  (agg->expr opts (AggregatorFactory/createAvg dist? arg)))

(defmethod ast-node->jena-expr 'sample
  [{dist? :distinct? :or {dist? false} :as opts} op [arg :as args]]
  (assert-no-nested-aggs op args)
  (agg->expr opts (AggregatorFactory/createSample dist? arg)))

(defmethod ast-node->jena-expr 'count
  [{dist? :distinct? :or {dist? false} :as opts} op [arg :as args]]
  (or (= :* arg)
      (assert-no-nested-aggs op args))
  (if (= :* arg)
    (agg->expr opts (AggregatorFactory/createCount dist?))
    (agg->expr opts (AggregatorFactory/createCountExpr dist? arg))))

(defmethod ast-node->jena-expr 'group-concat
  [{dist? :distinct?
    ?sep  :separator
    :or   {dist? false}
    :as   opts}
   op
   [arg :as args]]
  (assert-no-nested-aggs op args)
  (agg->expr opts (AggregatorFactory/createGroupConcat dist? arg ?sep nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The real Jena parser uses a registry for aggregates, so we copy that
;; strategy with the `aggregates` opt arg.

(defn- throw-unregistered-agg
  [fn-uri]
  (throw (ex-info (-> (str "Custom function '%s' not registered as aggregate; "
                           "cannot use 'distinct?' keyword.")
                      (format fn-uri))
                  {:kind ::unregistered-aggregate
                   :uri  fn-uri})))

(defmethod ast-node->jena-expr :custom
  [{aggs  :aggregate-fns
    query :query
    ?dist :distinct?}
   ^Node fn-op
   ^List fn-args]
  (let [fn-uri    (.getURI fn-op)
        expr-list (ExprList. fn-args)]
    (cond
      ;; Custom Aggregate
      (contains? aggs fn-uri)
      (do (assert-no-nested-aggs fn-uri fn-args)
          (assert-query query)
          (->> expr-list
               (AggregatorFactory/createCustom fn-uri (boolean ?dist))
               (.allocAggregate query)))
      ;; Custom Non-aggregate
      (nil? ?dist)
      (E_Function. fn-uri expr-list)
      :else
      (throw-unregistered-agg fn-uri))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Putting it all together
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod ast/ast-node->jena :expr/op [_ [_ op]] op)

(defmethod ast/ast-node->jena :expr/args [_ [_ args]] args)

(defmethod ast/ast-node->jena :expr/kwarg [_ [_ [[_ k] [_ v]]]] [k v])

(defmethod ast/ast-node->jena :expr/kwargs [_ [_ kwargs]]
  (into {} kwargs))

(defmethod ast/ast-node->jena :expr/terminal [_ [_ terminal-node]]
  (cond
    (= :* terminal-node)
    terminal-node
    (.isVariable ^Node terminal-node)
    (ExprVar. (Var/alloc ^Node terminal-node))
    :else
    (NodeValue/makeNode ^Node terminal-node)))

(defmethod ast/ast-node->jena :expr/branch
  [opts [_ [op args ?kwargs]]]
  (ast-node->jena-expr (merge opts ?kwargs) op args))

(defmethod ast/ast-node->jena :expr/as-var [_ [_ [expr var]]]
  (ElementBind. var expr))
