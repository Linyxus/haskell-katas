{ mkDerivation, base, containers, lens, lib, mtl }:
mkDerivation {
  pname = "katas";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers lens mtl ];
  license = lib.licenses.bsd3;
}
