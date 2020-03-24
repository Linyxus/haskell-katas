update:
	cabal2nix . > katas.nix

shell: update
	nix-shell

run: update
	nix-shell --run "cabal configure; cabal run"

build: update
	nix-build release.nix

clean:
	rm -rf cabal.*
	rm -rf dist*
	rm -rf result
