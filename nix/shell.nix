{ docker-client
, gh
, git
, jq
, nil
, nixpkgs-fmt
, nodePackages
, upx
, callPackage
, ...
}:
let
  ## Our haskell:
  thisHaskell = callPackage ./haskell.nix { };
in
thisHaskell.shellFor {
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
    docker-client
    gh
    git
    jq
    nil
    nixpkgs-fmt
    nodePackages.prettier
    upx

    ## Our custom development stuff:
    (callPackage ./dev-build-static { })
    (callPackage ./dev-test-build { })
  ];
}

