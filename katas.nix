{ mkDerivation, base, containers, lens, megaparsec, mtl, random
, stdenv, text
}:
mkDerivation {
  pname = "katas";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers lens megaparsec mtl random text
  ];
  license = stdenv.lib.licenses.bsd3;
}
