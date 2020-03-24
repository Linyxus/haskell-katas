{ mkDerivation, base, containers, lens, mtl, random, stdenv }:
mkDerivation {
  pname = "katas";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers lens mtl random ];
  license = stdenv.lib.licenses.bsd3;
}
