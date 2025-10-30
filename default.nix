{ mkDerivation, base, lib }:
mkDerivation {
  pname = "gnuplot-qq";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
}
