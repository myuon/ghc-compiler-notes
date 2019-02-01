format:
	find src/ -type f -name "*hs" | xargs floskell
	find tests/ -type f -name "*hs" -not -path 'tests/resources/*' | xargs floskell
	find app/ -type f -name "*hs" | xargs floskell
.PHONY: format
