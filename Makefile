.PHONY: build
build:
	cabal new-build

.PHONY: format
format:
	find app src test -type f \( -name "*.hs" -or -name "*.lhs" \) | xargs -I{} sh -c "floskell {} || true"

.PHONY: test
test:
	cabal new-test

.PHONY: exec
exec:
	cabal new-build && cabal new-exec -- ghc-compiler-notes conf/ghc-8.6.4.yml

.PHONY: generate
generate: exec
	cd sphinx-docs/ && make html BUILDIDR=../docs && cd -
	git add . && git commit -m 'build html'

.PHONY: clean
clean:
	rm -rf docs/
	mkdir docs/
	rm -rf sphinx-docs/source/notes
