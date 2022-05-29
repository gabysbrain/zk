
.PHONY: all build test

all: build test

build:
	nix develop -c cabal build

test:
	nix develop -c cabal test

