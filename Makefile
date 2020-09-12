build: client-build server-build

client-build:
	(cd client ; make)

server-build:
	stack build

install:
	stack install

.PHONY: reverse-proxy
reverse-proxy:
	nginx -c $(shell pwd)/nginx.conf

.PHONY: server
server:
	stack exec chordsheetify-exe

.PHONY: client
client:
	(cd client/ ; yarn start)
