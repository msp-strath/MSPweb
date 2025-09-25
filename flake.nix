{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-25.05;
  inputs.flake-utils.url = github:numtide/flake-utils;
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        MSPweb = (pkgs.haskellPackages.callPackage ./MSPweb.nix { });
      in
        rec {
          packages.default = MSPweb;
          devShells.default = MSPweb.env;
          devShells.full =
            pkgs.haskellPackages.shellFor {
              packages = hpkgs: [ MSPweb ];
              nativeBuildInputs = [
                pkgs.cabal-install
                pkgs.haskell-language-server
                pkgs.cabal2nix
                pkgs.gnumake
              ];
            };
        });
}
