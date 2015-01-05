default: lib

lib:
	cabal build

test: install-dependencies
	cabal test --show-details=always --test-option=--color

install-dependencies: list-prompt.cabal
	cabal install --only-dep --enable-test -j4
