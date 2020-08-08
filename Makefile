package = ipfs

stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

setup:
	stack install ghcid

build:
	$(stack) build --fast $(package):lib

release:
	$(stack) build

docs:
	$(stack) haddock $(package) --open

docserver:
	http-server ./.stack-work/dist/x86_64-osx/Cabal-3.0.1.0/doc/html/$(package) -p 1313 & \
    open http://localhost:1313

doctest:
	$(stack) test :fission-doctest --fast

unit-test:
	$(stack) test :fission-test --fast

test:
	make unit-test && make doctest

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests --ghci-options='-j6 +RTS -A128m'

bench:
	$(stack) build --bench $(package)

dev:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test"

live:
	$(stack) exec -- yesod devel

.PHONY : build dirty run install ghci test test-ghci watch doctest lint
