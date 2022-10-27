# flint-jena

A companion library to [Flint](https://github.com/yetanalytics/flint) for the direct compilation of Flint data into [Apache Jena](https://jena.apache.org/)-based SPARQL queries and updates.

## Installation

```clojure
{com.yetanalytics/flint-jena {:mvn/version "0.1.0"
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
