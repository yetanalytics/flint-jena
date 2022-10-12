(ns com.yetanalytics.flint-jena.expr-test
  (:require [clojure.test       :refer [deftest testing is are]]
            [clojure.spec.alpha :as s]
            [com.yetanalytics.flint-jena]
            [com.yetanalytics.flint-jena.ast   :as ast]
            [com.yetanalytics.flint-jena.axiom :as ax]
            [com.yetanalytics.flint.spec.expr  :as es])
  (:import [org.apache.jena.sparql.core Prologue]
           [org.apache.jena.sparql.expr Expr]))

(def prologue
  (doto (Prologue.)
    (.setPrefix "xsd" "http://www.w3.org/2001/XMLSchema#")))

(defn- expr->str
  [expr]
  (->> expr
       (s/conform ::es/expr)
       ^Expr (ast/ast->jena {:prologue      prologue
                             :iri->datatype ax/xsd-datatype-map})
       .toString))

(defn- agg-expr->str
  [agg-expr]
  (->> agg-expr
       (s/conform ::es/agg-expr)
       ^Expr (ast/ast->jena {:prologue      prologue
                             :iri->datatype ax/xsd-datatype-map})
       .toString))

(deftest expression-test
  (testing "Nilary expressions"
    (are [expr-str expr]
         (= expr-str
            (expr->str expr)
            (agg-expr->str expr))
      "(rand)"    '(rand)
      "(now)"     '(now)
      "(uuid)"    '(uuid)
      "(struuid)" '(struuid)
      "(bnode)"   '(bnode)))
  (testing "Unary expressions"
    (are [expr-str expr]
         (= expr-str
            (expr->str expr)
            (agg-expr->str expr))
      "(bnode \"x\")"       '(bnode "x")
      "(! false)"           '(not false)
      "(datatype \"x\")"    '(datatype "x")
      "(lang \"x\"@en)"     '(lang {:en "x"})
      "(str \"9001\")"      '(str "9001")
      "(strlen \"x\")"      '(strlen "x")
      "(ucase \"uwu\")"     '(ucase "uwu")
      "(lcase \"OWO\")"     '(lcase "OWO")
      "(encode_for_uri ?x)" '(encode-for-uri ?x)
      "(iri ?iri)"          '(iri ?iri)
      "(iri ?uri)"          '(uri ?uri)
      "(isBlank ?bnode)"    '(blank? ?bnode)
      "(isLiteral \"lit\")" '(literal? "lit")
      "(isNumeric ?n)"      '(numeric? ?n)
      "(isIRI ?iri)"        '(iri? ?iri)
      "(isIRI ?uri)"        '(uri? ?uri)
      "(abs ?n)"            '(abs ?n)
      "(ceil ?n)"           '(ceil ?n)
      "(floor ?n)"          '(floor ?n)
      "(round ?n)"          '(round ?n)
      "(year ?dt)"          '(year ?dt)
      "(month ?dt)"         '(month ?dt)
      "(day ?dt)"           '(day ?dt)
      "(hours ?dt)"         '(hours ?dt)
      "(minutes ?dt)"       '(minutes ?dt)
      "(seconds ?dt)"       '(seconds ?dt)
      "(timezone ?dt)"      '(timezone ?dt)
      "(tz ?dt)"            '(tz ?dt)
      "(MD5 \"x\")"         '(md5 "x")
      "(SHA1 \"x\")"        '(sha1 "x")
      "(SHA256 \"x\")"      '(sha256 "x")
      "(SHA384 \"x\")"      '(sha384 "x")
      "(SHA512 \"x\")"      '(sha512 "x")
      "(bound ?x)"          '(bound ?x)))
      ;; TODO: exists and not-exists exprs
  (testing "Binary expressions"
    (are [expr-str expr]
         (= expr-str
            (expr->str expr)
            (agg-expr->str expr))
      "(langMatches ?x \"en\")"   '(lang-matches ?x "en")
      "(contains \"foo\" \"f\")"  '(contains "foo" "f")
      "(strlang \"foo\" \"en\")"  '(strlang "foo" "en")
      "(strdt \"2\" <x:dtype>)"   '(strdt "2" "<x:dtype>")
      "(strstarts \"foo\" \"f\")" '(strstarts "foo" "f")
      "(strends \"foo\" \"o\")"   '(strends "foo" "o")
      "(strbefore \"bar\" \"a\")" '(strbefore "bar" "a")
      "(strafter \"bar\" \"a\")"  '(strafter "bar" "a")
      "(sameTerm ?x ?y)"          '(sameterm ?x ?y)
      "(= ?x ?y)"                 '(= ?x ?y)
      "(!= ?x ?y)"                '(not= ?x ?y)
      "(< ?x ?y)"                 '(< ?x ?y)
      "(> ?x ?y)"                 '(> ?x ?y)
      "(<= ?x ?y)"                '(<= ?x ?y)
      "(>= ?x ?y)"                '(>= ?x ?y)))
  (testing "Binary-plus expressions"
    (are [expr-str expr]
         (= expr-str
            (expr->str expr)
            (agg-expr->str expr))
      "(+ (+ ?a ?b) ?c)"   '(+ ?a ?b ?c)
      "(- (- ?a ?b) ?c)"   '(- ?a ?b ?c)
      "(* (* ?a ?b) ?c)"   '(* ?a ?b ?c)
      "(/ (/ ?a ?b) ?c)"   '(/ ?a ?b ?c)
      "(&& (&& ?a ?b) ?c)" '(and ?a ?b ?c)
      "(|| (|| ?a ?b) ?c)" '(or ?a ?b ?c)
      "(in ?x ?a ?b ?c)"   '(in ?x ?a ?b ?c)
      "(notin ?x ?a ?b)"   '(not-in ?x ?a ?b)))
  (testing "Misc-arity expressions"
    (are [expr-str expr]
         (= expr-str
            (expr->str expr)
            (agg-expr->str expr))
      "(regex \"foo\" \".*\")"              '(regex "foo" ".*")
      "(regex \"foo\" \".*\" \"i\")"        '(regex "foo" ".*" "i")
      "(substr \"foo\" ?s)"                 '(substr "foo" ?s)
      "(substr \"foo\" ?s ?e)"              '(substr "foo" ?s ?e)
      "(if ?x ?then ?else)"                 '(if ?x ?then ?else)
      "(replace \"foo\" \"f\" \"g\")"       '(replace "foo" "f" "g")
      "(replace \"foo\" \"f\" \"g\" \"i\")" '(replace "foo" "f" "g" "i")
      "(concat \"foo\" \"-\" \"bar\")"      '(concat "foo" "-" "bar")
      "(coalesce ?x \"boo\")"               '(coalesce ?x "boo")))
  (testing "Nested expressions"
    (is (= "(* (+ \"2\"^^<http://www.w3.org/2001/XMLSchema#long> \"3\"^^<http://www.w3.org/2001/XMLSchema#long>) \"4\"^^<http://www.w3.org/2001/XMLSchema#long>)"
           (-> '(* (+ 2 3) 4) expr->str)
           (-> '(* (+ 2 3) 4) agg-expr->str)))
    (is (= "(! (|| true (&& false false)))"
           (-> '(not (or true (and false false))) expr->str)
           (-> '(not (or true (and false false))) agg-expr->str))))
  (testing "Aggregate expressions"
    (are [expr-str expr]
         (= expr-str (agg-expr->str expr))
      "SUM(?x)"    '(sum ?x)
      "MIN(?x)"    '(min ?x)
      "MAX(?x)"    '(max ?x)
      "AVG(?x)"    '(avg ?x)
      "SAMPLE(?x)" '(sample ?x)
      "COUNT(?x)"  '(count ?x)
      "count(*)"   '(count *)
      "SUM(DISTINCT ?x)"    '(sum ?x :distinct? true)
      "MIN(DISTINCT ?x)"    '(min ?x :distinct? true)
      "MAX(DISTINCT ?x)"    '(max ?x :distinct? true)
      "AVG(DISTINCT ?x)"    '(avg ?x :distinct? true)
      "SAMPLE(DISTINCT ?x)" '(sample ?x :distinct? true)
      "COUNT(DISTINCT ?x)"  '(count ?x :distinct? true)
      "count(distinct *)"   '(count * :distinct? true)
      "GROUP_CONCAT(?x)"                          '(group-concat ?x)
      "GROUP_CONCAT(?x ; separator=';')"          '(group-concat ?x :separator ";")
      "GROUP_CONCAT(DISTINCT ?x)"                 '(group-concat ?x :distinct? true)
      "GROUP_CONCAT(DISTINCT ?x ; separator=';')" '(group-concat ?x :distinct? true :separator ";")))
  (testing "Custom expressions"
    (is (= "(<http://fn.com> ?x ?y)"
           (->> '("<http://fn.com>" ?x ?y)
                (s/conform ::es/expr)
                ^Expr (ast/ast->jena {:prologue      prologue
                                      :iri->datatype ax/xsd-datatype-map})
                .toString)))
    (is (= "AGG <http://fn.com>(?x , ?y))"
           (->> '("<http://fn.com>" ?x ?y)
                (s/conform ::es/agg-expr)
                ^Expr (ast/ast->jena {:prologue      prologue
                                      :iri->datatype ax/xsd-datatype-map
                                      :aggregate-fns #{"http://fn.com"}})
                .toString)))
    (is (= "AGG <http://fn.com>(DISTINCT ?x , ?y)"
           (->> '("<http://fn.com>" ?x ?y :distinct? true)
                (s/conform ::es/agg-expr)
                ^Expr (ast/ast->jena {:prologue      prologue
                                      :iri->datatype ax/xsd-datatype-map
                                      :aggregate-fns #{"http://fn.com"}})
                .toString)))
    (is (= "Custom function 'http://fn.com' not registered as aggregate; cannot use 'distinct?' keyword."
           (try (->> '("<http://fn.com>" ?x ?y :distinct? true)
                     (s/conform ::es/agg-expr)
                     ^Expr (ast/ast->jena {:prologue      prologue
                                           :iri->datatype ax/xsd-datatype-map
                                           :aggregate-fns #{}}))
                (catch IllegalArgumentException e
                  (.getMessage e)))))))
