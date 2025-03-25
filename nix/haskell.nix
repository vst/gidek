{ haskellPackages
, ...
}:

haskellPackages.override {
  overrides = self: _super: {
    gidek = self.callCabal2nix "gidek" ../. { };
  };
}
