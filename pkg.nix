{ mkDerivation, base, binary, bytestring, formatting, hspec, lens
, mtl, stdenv, text, vector
}:
mkDerivation {
  pname = "LC3";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base binary bytestring formatting hspec lens mtl text vector
  ];
  description = "LC-3 virtual machine";
  license = stdenv.lib.licenses.bsd3;
}
