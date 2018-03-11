{ mkDerivation, base, foldl, HUnit, lens, pipes, stdenv
, test-framework, test-framework-hunit, transformers
}:
mkDerivation {
  pname = "pipes-extras";
  version = "1.0.13";
  src = ./.;
  libraryHaskellDepends = [ base foldl lens pipes transformers ];
  testHaskellDepends = [
    base HUnit pipes test-framework test-framework-hunit transformers
  ];
  description = "Extra utilities for pipes";
  license = stdenv.lib.licenses.bsd3;
}
