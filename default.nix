{ mkDerivation, base, foldl, HUnit, pipes, stdenv, test-framework
, test-framework-hunit, transformers
}:
mkDerivation {
  pname = "pipes-extras";
  version = "1.0.8";
  src = ./.;
  libraryHaskellDepends = [ base foldl pipes transformers ];
  testHaskellDepends = [
    base HUnit pipes test-framework test-framework-hunit transformers
  ];
  description = "Extra utilities for pipes";
  license = stdenv.lib.licenses.bsd3;
}
