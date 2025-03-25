{
  ## Production Dependencies:
  bashInteractive
, gh
, git
, jq

  ## Build Dependencies:
, installShellFiles
, makeWrapper
, lib
, haskell
, callPackage
, ...
}:
let
  ## Define the name:
  name = "gidek";

  ## Our haskell:
  thisHaskell = callPackage ./haskell.nix { };

  ## We need these inputs at buildtime:
  extraNativeBuildInputs = [
    git
    installShellFiles
    makeWrapper
  ];

  ## We need these inputs at runtime:
  binPath = lib.makeBinPath [
    bashInteractive
    gh
    git
    jq
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
haskell.lib.justStaticExecutables (
  thisHaskell.gidek.overrideAttrs (oldAttrs: {
    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ extraNativeBuildInputs;
    postFixup = (oldAttrs.postFixup or "") + extraPostFixup;
  })
)
