{
  inputs = {
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.1.0";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
  };

  outputs = { flake-utils, nixpkgs, ... }: {
    nixosModules = rec {
      gidek = ./nix/modules/nixos;
      default = gidek;
    };
  } // flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShell = pkgs.callPackage ./nix/shell.nix { };
      packages = rec {
        gidek = pkgs.callPackage ./nix/package.nix { };
        default = gidek;
      };
    });
}
