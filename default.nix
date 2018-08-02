{ mkDerivation, aeson, aeson-better-errors, base, bytestring
, datetime, either, hashmap, mtl, optparse-applicative, random
, semigroups, stdenv, text, transformers
}:
mkDerivation {
  pname = "QuickData";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-better-errors base bytestring datetime either hashmap
    mtl optparse-applicative random semigroups text transformers
  ];
  executableHaskellDepends = [ base ];
  description = "Quick Dummy Data for SQL";
  license = stdenv.lib.licenses.bsd3;
}
