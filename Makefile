.phony: test-clj test-cov ci

test-clj:
	clj -X:test:run

test-cov:
	clj -X:test:cov

ci: test-clj
