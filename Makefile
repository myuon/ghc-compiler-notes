run:
	stack build --fast && stack exec gen-docs ghc/compiler

serve:
	docsify serve ./docs

clean:
	rm -r docs
	mkdir docs
	docsify init ./docs

