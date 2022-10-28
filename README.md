# flint-jena

[![CI](https://github.com/yetanalytics/flint-jena/actions/workflows/test.yml/badge.svg)](https://github.com/yetanalytics/flint-jena/actions/workflows/test.yml)
[![Clojars Project](https://img.shields.io/clojars/v/com.yetanalytics/flint-jena.svg)](https://clojars.org/com.yetanalytics/flint)
[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-5e0b73.svg)](CODE_OF_CONDUCT.md)

A companion library to [Flint](https://github.com/yetanalytics/flint) for the direct compilation of Flint data into [Apache Jena](https://jena.apache.org/)-based SPARQL queries and updates.

Uses Flint version [v0.2.1](https://github.com/yetanalytics/flint/releases/tag/v0.2.1).

## Installation

```clojure
{com.yetanalytics/flint-jena {:mvn/version "0.1.1"
                              :exclusions [org.clojure/clojure]}}
```

## Why flint-jena?

Vanilla Flint API functions (e.g. `format-query` and `format-updates`) return SPARQL strings, which are useful for many applications; however, within Apache Jena it is more useful to work directly with [`Query`](https://www.javadoc.io/doc/org.apache.jena/jena-arq/latest/org.apache.jena.arq/org/apache/jena/query/Query.html) and [`UpdateRequest`](https://www.javadoc.io/doc/org.apache.jena/jena-arq/latest/org.apache.jena.arq/org/apache/jena/update/UpdateRequest.html) objects. Parsing from Flint to SPARQL strings back to Jena incurs a performance hit compared to directly instantiating the Jena objects, which this library seeks to avoid.

## API Functions

There are two API functions:
- `create-query` accepts a single Flint query and returns a `Query` instance.
- `create-updates` accepts a collection of Flint updates and returns an `UpdateRequest` instance.

Both API functions have the following keyword arguments:
- `spec-ed?`: If `true`, then the entire Clojure spec error data is displayed upon a syntax error. Default `false`.
- `iri->datatype`: A function from SPARQL literal datatype IRIs to Jena datatype classes, for coercing [typed literals](https://jena.apache.org/documentation/notes/typed-literals.html). The map `axiom/xsd-datatype-map` is the default value.\*
- `aggregate-fns`: A set of function IRI strings (_not_ wrapped, so `\"http://fn.com\"` is a valid entry) to be treated as custom [aggregate expressions](https://jena.apache.org/documentation/query/group-by.html), as opposed to run-of-the-mill non-aggregate expressions.

\* There is also an `axiom/xsd-datatype-map*` map; the main difference is that there is a more one-to-one mapping of numeric datatype IRIs to classes, whereas `axiom/xsd-datatype-map` coerces them to either `xsd:integer` or `xsd:decimal`.

## Benchmarking (dev only)

The flint-jena repo comes with benchmarking utilities to bench `create-query` and `create-updates` against the vanilla Flint API functions, specifically the `format-*` functions followed by Jena SPARQL parsing. To bench, clone this repo, then execute the following function:
```clojure
com.yetanalytics.flint-jena-bench/bench
```
The `bench` function accepts the following arguments:
- `:query-inputs`: A vector of file paths from which to read query EDN. Default: `["dev-resources/test-fixtures/query"]`.
- `:update-inputs`: A vector of file paths from which to read update EDN. Default: `["dev-resources/test-fixtures/query"]`. 
- `:query-output`: A file path string where to write the query bench output. Default: `"target/bench/query.txt"`
- `:update-output`: A file path string where to write the update bench output. Default: `"target/bench/update.txt"`

The namespace also contains `bench-queries` and `bench-updates` if one only wants to bench `create-query` and `create-updates`, respectively. There are also `bench`, `bench-queries`, and `bench-updates` Makefile targets for benching with default args.

An example of use:
```clojure
clojure -X:bench
  com.yetanalytics.flint-jena-bench/bench-queries 
  :query-inputs '["dev-resources/test-fixtures/query/select/select-1.edn" 
                  "dev-resources/test-fixtures/query/select/select-2.edn"]'
```

Which then outputs the following to `target/bench/query.txt`:

```
************************ Queries creation bench results (in µs) ************************

|        :file | :format-query | :create-query |  :difference | :percent |
|--------------+---------------+---------------+--------------+----------|
| select-1.edn |  61.69 ± 0.00 |  25.38 ± 0.00 | 36.30 ± 0.00 |   58.85% |
| select-2.edn |  91.55 ± 0.00 |  37.61 ± 0.00 | 53.93 ± 0.00 |   58.91% |
```
