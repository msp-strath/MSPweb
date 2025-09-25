{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-25.05;
  inputs.flake-utils.url = github:numtide/flake-utils;
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        MSPweb = (pkgs.haskellPackages.callPackage ./MSPweb.nix { });
      in
        {
          packages.default = MSPweb;
          devShells.default = MSPweb.env;
        });
}
