# Backup Git(Hub) Repositories

> **TODO** Provide minimum viable documentation.

`gidek` lets you backup Git repositories hosted (for now only) on
[GitHub].

The idea is that you specify a list of GitHub repositories to backup
and `gidek` keeps cloning them everytime it is invoked. The list of
GitHub repositories of interest can be specified as:

1. Specific GitHub repositories by their `[owner]/[repository-name]`
   handle, or
2. All GitHub repositories which belong to specific GitHub users or
   organizations, ie. owners.


## Development

Provision `direnv`:

```sh
direnv allow
```

Big, long build command for the impatient:

```sh
hpack &&
    direnv reload &&
    fourmolu -i app/ src/ test/ &&
    prettier --write . &&
    find . -iname "*.nix" -not -path "*/nix/sources.nix" -print0 | xargs --null nixpkgs-fmt &&
    hlint app/ src/ test/ &&
    cabal build -O0 &&
    cabal run -O0 gidek -- --version &&
    cabal v1-test &&
    cabal haddock -O0
```

<!-- REFERENCES -->

[GitHub]: https://github.com
