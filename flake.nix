{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
  };

  outputs = { flake-utils, nixpkgs, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      ## Import nixpkgs:
      pkgs = import nixpkgs { inherit system; };

      ## Get this Haskell package set:
      thisHaskell = pkgs.haskellPackages.override {
        overrides = self: _super: {
          gidek = self.callCabal2nix "gidek" ./. { };
        };
      };
    in
    {
      devShell = thisHaskell.shellFor {
        ## Define packages for the shell:
        packages = p: [ p.gidek ];

        ## Enable Hoogle:
        withHoogle = false;

        ## Build inputs for development shell:
        buildInputs = [
          ## Haskell related build inputs:
          thisHaskell.apply-refact
          thisHaskell.cabal-fmt
          thisHaskell.cabal-install
          thisHaskell.cabal2nix
          thisHaskell.fourmolu
          thisHaskell.haskell-language-server
          thisHaskell.hlint
          thisHaskell.hpack
          thisHaskell.weeder

          ## Other build inputs for various development requirements:
          pkgs.docker-client
          pkgs.gh
          pkgs.git
          pkgs.jq
          pkgs.nil
          pkgs.nixpkgs-fmt
          pkgs.nodePackages.prettier
          pkgs.upx

          ## Our custom development stuff:
          (pkgs.callPackage ./nix/dev-build-static { })
          (pkgs.callPackage ./nix/dev-test-build { })
        ];
      };

      packages = rec {
        default = gidek;
        gidek =
          let
            ## Define the name:
            name = "gidek";

            ## We need these inputs at buildtime:
            extraNativeBuildInputs = [
              pkgs.git
              pkgs.installShellFiles
              pkgs.makeWrapper
            ];

            ## We need these inputs at runtime:
            binPath = pkgs.lib.makeBinPath [
              pkgs.bashInteractive
              pkgs.gh
              pkgs.git
              pkgs.jq
            ];

            ## Post-fixup process:
            extraPostFixup = ''
              ## Wrap program:
              wrapProgram $out/bin/${name} --prefix PATH : ${binPath}

              ## Install completion scripts:
              installShellCompletion --bash --name  ${name}.bash <($out/bin/${name} --bash-completion-script "$out/bin/${name}")
              installShellCompletion --fish --name  ${name}.fish <($out/bin/${name} --fish-completion-script "$out/bin/${name}")
              installShellCompletion --zsh  --name _${name}      <($out/bin/${name} --zsh-completion-script  "$out/bin/${name}")
            '';
          in
          pkgs.haskell.lib.justStaticExecutables (
            thisHaskell.gidek.overrideAttrs (oldAttrs: {
              nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ extraNativeBuildInputs;
              postFixup = (oldAttrs.postFixup or "") + extraPostFixup;
            })
          );
      };
    });
}
