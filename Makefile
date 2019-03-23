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
	cabal new-build && cabal new-exec -- ghc-compiler-notes conf/ghc-8.6.4.ym
	cd sphinx-docs/ && make html && cd -
	cp -r sphinx-docs/build/html docs
	git add . && git commit -m 'build html'
