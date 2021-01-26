update:
	cabal2nix . > project.nix

shell: update
	nix-shell

run: update
	nix-shell --run "cabal configure; cabal run"

build: update
	nix-build .

clean:
	rm -rf cabal.*
	rm -rf dist*
	rm -rf result
