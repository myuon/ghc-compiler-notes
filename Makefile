.PHONY: build
build:
	cabal new-build

.PHONY: format
format:
	find app src test -type f \( -name "*.hs" -or -name "*.lhs" \) | xargs -I{} sh -c "floskell {} || true"

.PHONY: test
test:
	cabal new-test
