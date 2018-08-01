{ mkDerivation, base, HUnit, old-locale, old-time, QuickCheck
, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, time
}:
mkDerivation {
  pname = "datetime";
  version = "0.3.1";
  sha256 = "0jmxxmv5s9rch84ivfjhqxdqnvqqzvabjs152wyv47h5qmvpag1k";
  libraryHaskellDepends = [ base old-locale old-time time ];
  testHaskellDepends = [
    base HUnit old-locale old-time QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 time
  ];
  homepage = "http://github.com/stackbuilders/datetime";
  description = "Utilities to make Data.Time.* easier to use";
  license = "GPL";
}
