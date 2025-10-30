{ pkgs, hspkgs }:
hspkgs.shellFor {
  withHoogle = true;
  packages = p: [ p.gnuplot-qq ];
  buildInputs = [
    hspkgs.cabal-install
    hspkgs.haskell-language-server
    hspkgs.hlint
    hspkgs.cabal2nix
    pkgs.ghcid
    pkgs.gnuplot_qt
  ];
}
