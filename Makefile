.phony: test-clj test-cov ci

test:
	clojure -X:test:run

test-cov:
	clojure -X:test:cov

ci: test
