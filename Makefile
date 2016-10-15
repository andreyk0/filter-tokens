build:
	stack build filter-tokens

build-prof:
	stack build --profile --ghc-options="-rtsopts" filter-tokens 

install:
	stack install filter-tokens

clean:
	stack clean

tags:
	hasktags-generate .

sources:
	stack-unpack-dependencies

.PHONY: build build-prof clean tags sources

