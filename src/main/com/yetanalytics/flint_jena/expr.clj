(ns com.yetanalytics.flint-jena.expr
  (:require [com.yetanalytics.flint-jena.ast :as ast])
  (:import [java.util List]
           [org.apache.jena.graph Node]
           [org.apache.jena.sparql.core Var]
           [org.apache.jena.sparql.syntax Element]
           [org.apache.jena.sparql.expr.aggregate AggregatorFactory]
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
            ExprList
            ExprVar
            NodeValue]))

(defn- jena-expr-dispatch [_opts op _args]
  (if (symbol? op) op :custom))

(defmulti ast-node->jena-expr jena-expr-dispatch)

;; Protocol impls are defined in other namespaces via extend-protocol.
(defrecord ExprAsVar [expression variable])

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

(defn- defvarardic-err-msg
  [sym]
  (format "Expression '%s' cannot have less than 2 arguments!" sym))

(defmacro defvaradic [sym class-name]
  `(defmethod ast-node->jena-expr ~sym
     [~'opts ~'op ~'args]
     (cond
       (< (count ~'args) 2)
       (throw (IllegalArgumentException. ~(defvarardic-err-msg sym)))
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

(defunary 'iri E_IRI)
(defunary 'uri E_IRI)

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

(defunary 'bound      E_Bound)

;; Need to define the defmethod bodies manually since the E_Exists and
;; E_NotExists constructors are overloaded and require type hitns.

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

(defbinary 'sameterm     E_SameTerm)
(defbinary '=    E_Equals)
(defbinary 'not= E_NotEquals)
(defbinary '<    E_LessThan)
(defbinary '>    E_GreaterThan)
(defbinary '<=   E_LessThanOrEqual)
(defbinary '>=   E_GreaterThanOrEqual)

;; Binary+ expressions

(defvaradic 'and E_LogicalAnd)
(defvaradic 'or  E_LogicalOr)
(defvaradic '+   E_Add)
(defvaradic '-   E_Subtract)
(defvaradic '*   E_Multiply)
(defvaradic '/   E_Divide)

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

(defmethod ast-node->jena-expr 'sum
  [{dist? :distinct? :or {dist? false}} _ args]
  (AggregatorFactory/createSum dist? (first args)))

(defmethod ast-node->jena-expr 'min
  [{dist? :distinct? :or {dist? false}} _ args]
  (AggregatorFactory/createMin dist? (first args)))

(defmethod ast-node->jena-expr 'max
  [{dist? :distinct? :or {dist? false}} _ args]
  (AggregatorFactory/createMax dist? (first args)))

(defmethod ast-node->jena-expr 'avg
  [{dist? :distinct? :or {dist? false}} _ args]
  (AggregatorFactory/createAvg dist? (first args)))

(defmethod ast-node->jena-expr 'sample
  [{dist? :distinct? :or {dist? false}} _ args]
  (AggregatorFactory/createSample dist? (first args)))

(defmethod ast-node->jena-expr 'count
  [{dist? :distinct? :or {dist? false}} _ args]
  (if (= :* (first args))
    (AggregatorFactory/createCount dist?)
    (AggregatorFactory/createCountExpr dist? (first args))))

(defmethod ast-node->jena-expr 'group-concat
  [{dist? :distinct?
    ?sep  :separator
    :or   {dist? false}} _ args]
  (AggregatorFactory/createGroupConcat dist? (first args) ?sep nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The real Jena parser uses a registry for aggregates, so we copy that
;; strategy with the `aggregates` opt arg.

(defmethod ast-node->jena-expr :custom
  [{aggs  :aggregate-fns
    ?dist :distinct?}
   ^Node fn-op
   ^java.util.List fn-args]
  (let [fn-uri    (.getURI fn-op)
        expr-list (ExprList. fn-args)]
    (cond
      ;; Custom Aggregate
      (contains? aggs fn-uri)
      (AggregatorFactory/createCustom fn-uri (boolean ?dist) expr-list)
      ;; Custom Non-aggregate
      (nil? ?dist)
      (E_Function. fn-uri expr-list)
      :else
      (throw (IllegalArgumentException.
              (format (str "Custom function '%s' not registered as aggregate; "
                           "cannot use 'distinct?' keyword.")
                      fn-uri))))))

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
  (->ExprAsVar expr var))

(comment
  (.isVariable
   (ast/ast->jena {}
                [:ax/var '?bar]))
  (ast/ast->jena {}
               [:expr/op '-])
  (ast/ast->jena {}
               [:expr/args
                [[:ax/var '?foo]
                 [:ax/var '?bar]]])
  (ast/ast->jena {}
               [:expr/branch
                [[:expr/op '=]
                 [:expr/args
                  [[:expr/terminal [:ax/var '?foo]]
                   [:expr/terminal [:ax/var '?bar]]]]]])
  
  (ast/ast->jena {}
               [:expr/branch
                [[:expr/op 'regex]
                 [:expr/args
                  [[:expr/terminal [:ax/var '?foo]]
                   [:expr/terminal [:ax/literal ".*"]]
                   #_[:expr/terminal [:ax/literal "s"]]]]]])
  )
