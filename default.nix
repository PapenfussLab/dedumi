{
  cloudflareZlib ? (builtins.fetchTarball {
    url = "https://github.com/cloudflare/zlib/archive/d20bdfcd0efbdd72cb9d857e098ceac1bad41432.tar.gz";
    sha256 = "sha256-KNS1FDznR2UaXo0WBOUh4lnVY23Kc9JnNyVtYzSpWDw=";
  }),
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
      zlib = super.zlib.override {zlib = pkgs.zlib.overrideAttrs (_: {src = cloudflareZlib;});};
    };
  };
in
  hp.callCabal2nix "dedumi" ./. {}
