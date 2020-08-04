build: client-build server-build

client-build:
	(cd client ; make)

server-build:
	stack build

install:
	stack install
