# gidek - Backup Git(Hub) Repositories

<div align="center">
  <a href="https://github.com/vst/gidek/releases/latest"><img alt="GitHub Release" src="https://img.shields.io/github/v/release/vst/gidek?style=for-the-badge" /></a>
  <a href="https://github.com/vst/gidek/actions/workflows/release.yaml"><img alt="GitHub Actions Workflow Status" src="https://img.shields.io/github/actions/workflow/status/vst/gidek/release.yaml?style=for-the-badge" /></a>
</div>

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

## Quickstart

A NixOS module is provided for both classic and flake-based Nix setups.
Add following to your configuration and edit it as per your needs:

```nix
  imports = [
    "${sources.gidek}/nix/modules/nixos"
  ];

  services.gidek = {
    enable = true;
    user = "vst";
    schedule = "Sat *-*-* 00:00:01";
  };

  programs.gidek = {
    enable = true;
    config = {
      store = "/data/gidek";
      token_file = config.sops.secrets.github_token.path;
      repos = [
        { type = "single"; name = "vst/gidek"; }
        # { type = "user"; name = "vst"; }
        # { type = "organization"; name = "fourmolu"; }
      ];
  };
```

Alternatively, clone the repository:

```sh
git clone git@github.com:vst/gidek.git
cd gidek
```

Using `nix profile`:

```sh
nix profile install --file default.nix app
```

Using `nix-env`

```sh
nix-env -i -f default.nix -A app
```

Create a configuration file:

```sh
cat <<EOF > config.yaml
store: /tmp/gidek/store
token: $(gh auth token)
token_file: /var/run/secrets/github_token
repos:
  - type: single
    name: vst/gidek
  - type: user
    name: vst
  - type: organization
    name: fourmolu
EOF
```

Plan backups:

```sh
gidek --config config.yaml plan
```

Run backups:

```sh
gidek --config config.yaml backup
```

## Development

Provision `direnv`:

```sh
direnv allow
```

Big, long build and test command for the impatient:

```sh
dev-test-build
```

<!-- REFERENCES -->

[GitHub]: https://github.com
