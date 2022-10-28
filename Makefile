.phony: test-clj test-cov ci

test:
	clojure -X:test:run

test-cov:
	clojure -X:test:cov

ci: test

# Benchmarking

.phony: bench bench-queries bench-updates

bench:
	clojure -X:bench com.yetanalytics.flint-jena-bench/bench

bench-queries:
	clojure -X:bench com.yetanalytics.flint-jena-bench/bench-queries

bench-updates:
	clojure -X:bench com.yetanalytics.flint-jena-bench/bench-updates
