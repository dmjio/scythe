{ mkDerivation, alex, array, base, bytestring, mtl, stdenv }:
mkDerivation {
  pname = "scythe";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ array base bytestring mtl ];
  libraryToolDepends = [ alex ];
  executableHaskellDepends = [ base bytestring ];
  homepage = "https://github.com/dmjio/scythe";
  description = "Fast CSV lexing on ByteString";
  license = stdenv.lib.licenses.bsd3;
}