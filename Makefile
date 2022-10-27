.phony: test-clj test-cov ci

test:
	clj -X:test:run

test-cov:
	clj -X:test:cov

ci: test
