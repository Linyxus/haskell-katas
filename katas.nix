{ mkDerivation, base, containers, lens, lib, mtl, random }:
mkDerivation {
  pname = "katas";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers lens mtl random ];
  license = lib.licenses.bsd3;
}
