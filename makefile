
.PHONY: all build test 

all: build test

zk.cabal: package.yaml
	nix develop -c hpack

build: zk.cabal
	nix develop -c cabal build

test: zk.cabal
	nix develop -c cabal test --test-option=--color

