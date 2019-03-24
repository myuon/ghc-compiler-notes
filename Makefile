.PHONY: build
build:
	cabal new-build

.PHONY: format
format:
	find app src test -type f \( -name "*.hs" -or -name "*.lhs" \) | xargs -I{} sh -c "floskell {} || true"

.PHONY: test
test:
	cabal new-test

.PHONY: generate
generate:
	rm -rf docs/notes
	cabal new-build && cabal new-exec -- ghc-compiler-notes conf/ghc-8.6.4.yml

.PHONY: serve-docs
serve-docs:
	sphinx-autobuild docs docs/_build
