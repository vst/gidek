{ sources ? import ./nix/sources.nix
, system ? builtins.currentSystem
, ...
}:

let
  ##################
  ## LOAD NIXPKGS ##
  ##################

  ## Import nixpkgs pinned by niv:
  pkgs = import sources.nixpkgs { inherit system; };

  ##################
  ## LOAD HELPERS ##
  ##################

  ## Load the YAML reader:
  readYAML = pkgs.callPackage ./nix/lib/read-yaml.nix { };

  ##############################
  ## LOAD PACKAGE INFORMATION ##
  ##############################

  ## Get the main Haskell package specification:
  packageSpec = readYAML ./package.yaml;

  #############
  ## HASKELL ##
  #############

  ## Get this Haskell package set:
  thisHaskell = pkgs.haskellPackages.override {
    overrides = self: _super: {
      "${packageSpec.name}" = self.callCabal2nix "${packageSpec.name}" ./. { };
    };
  };

  ###########
  ## SHELL ##
  ###########

  ## Prepare Nix shell:
  thisShell = thisHaskell.shellFor {
    ## Define packages for the shell:
    packages = p: [ p.${packageSpec.name} ];

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
      (pkgs.callPackage ./nix/dev-test-build { })
    ];
  };

  #################
  ## APPLICATION ##
  #################

  ## Get the installable application (only static executable):
  thisApp =
    let
      ## Name:
      name = packageSpec.name;

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
      thisHaskell.${packageSpec.name}.overrideAttrs (oldAttrs: {
        nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ extraNativeBuildInputs;
        postFixup = (oldAttrs.postFixup or "") + extraPostFixup;
      })
    );
in
{
  app = thisApp;
  shell = thisShell;
}
