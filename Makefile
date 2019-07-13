.PHONY: build open watch

build: 
		cabal v2-build --ghcjs --disable-optimization \
		&& cp -r public/* dist-newstyle/build/x86_64-linux/ghcjs-8.4.0.1/miso-plane-0.1.0.0/x/client/noopt/build/client/client.jsexe/

open:
	cd dist-newstyle/build/x86_64-linux/ghcjs-8.4.0.1/miso-plane-0.1.0.0/x/client/noopt/build/client/client.jsexe/ \
		&& npx browser-sync start --server --files "index.html" --single

watch:
	sos -p ".*hs\$$|.*cabal\$$" -c "make build"
