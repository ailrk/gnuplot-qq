{
  description = "gnuplot-qq";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/8f3cf34b8d2e2caf4ae5ee1d1fddc1baab4c5964";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, ... }@inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              gnuplot-qq = (hfinal.callPackage ./default.nix {}).overrideAttrs (old: {
                nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ final.upx ];
              });
            };
          };
        };

      perSystem = system:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ overlay ];
            config.stripDebugInfo = true; # strip all debug infos
          };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = {
            default = pkgs.callPackage ./shell.nix { hspkgs = hspkgs; };
          };

          packages = {
            default = hspkgs.gnuplot-qq;
          };
        };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
