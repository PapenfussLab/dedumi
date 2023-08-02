{
  pkgs ?
    import (builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/356c6dcdf37cfb4162f534e5dcabadddbfbd6bfa.tar.gz";
      sha256 = "sha256-JCkJfcZL1qs24L+Fv6OyFW4wTE6+NCg9I8nDf6npP+A=";
    }) {},
}: let
  inherit (pkgs.haskell.lib) dontCheck markUnbroken;
  hp = pkgs.haskell.packages.ghc928.override {
    overrides = self: super: rec {
      cuckoo = markUnbroken (dontCheck super.cuckoo);
    };
  };
in
  hp.callCabal2nix "dedumi" ./. {}
