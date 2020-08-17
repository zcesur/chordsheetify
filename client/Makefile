all: src/Api.elm build

src/Api.elm: GenerateElm.hs ../server/Api.hs
	(cd ../ ; stack exec -- runhaskell -iserver client/GenerateElm.hs)

.PHONY: build
build:
	yarn install
	yarn build
