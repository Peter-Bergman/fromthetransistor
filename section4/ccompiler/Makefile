BINFOLDER := $(shell pwd)/bin/
export BINFOLDER

.PHONY: default
default: Preprocessor

.PHONY: Preprocessor
Preprocessor:
	$(MAKE) -C $@ $@

.PHONY: Lexer
Lexer:
	$(MAKE) -C $@ $@
	#ghc -outputdir $(BINFOLDER) -o Lexer --make Lexer/Lexer.hs

.PHONY: CharTokenParsers
CharTokenParsers:
	$(MAKE) -C $@ $@

.PHONY: clean
clean:
	# Remove all object files recursively in this directory.
	find . -type f -name "**.o" -delete
	# Remove all the Haskell interface files recursively in this directory.
	find . -type f -name "**.hi" -delete
	# Remove everything inside of the bin directory.
	find bin/ -regex ".*bin/.+" -delete
	# Remove swap files
	find . -type f -regex ".*\.sw[A-Za-z]" -delete
