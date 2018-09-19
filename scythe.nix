{ mkDerivation, alex, array, base, bytestring, mtl, stdenv, text }:
mkDerivation {
  pname = "scythe";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ array base bytestring mtl text ];
  libraryToolDepends = [ alex ];
  executableHaskellDepends = [ base bytestring ];
  homepage = "https://github.com/dmjio/scythe";
  description = "Fast CSV lexing on ByteString";
  license = stdenv.lib.licenses.bsd3;
}
