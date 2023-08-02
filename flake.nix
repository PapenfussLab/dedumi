{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  outputs = {self, nixpkgs}: 
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {inherit system; config.allowBroken=true;};
      hp = pkgs.haskell.packages.ghc928.override {
        overrides = self: super: rec {
          cuckoo = pkgs.haskell.lib.dontCheck super.cuckoo;
        };
      };
    in
    {
        packages.${system}.default = hp.callCabal2nix "dedumi" ./. {};
        devShells.${system}.default = self.packages.${system}.default.env;
    };
}
