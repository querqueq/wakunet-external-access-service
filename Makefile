all: 
	cabal install --only-dependencies
	cabal configure
	cabal build
	cp -f dist/build/waku-external-access-service/waku-external-access-service bin/wakunet-external-access-service_x86_64

clean:
	rm -f bin/wakunet-external-access-service_x86_64
