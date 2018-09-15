run: ghc/.git docs docs/index.html
	stack build --fast && stack exec gen-docs ghc/compiler

docs:
	mkdir docs

docs/index.html:
	docsify init ./docs

serve:
	docsify serve ./docs

clean:
	rm -r docs

