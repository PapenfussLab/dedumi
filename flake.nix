{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.cloudflareZlib = {
    url = "github:cloudflare/zlib";
    flake = false;
  };
  inputs.inara = {
    url = "github:openjournals/inara";
    flake = false;
  };

  outputs = {
    self,
    nixpkgs,
    cloudflareZlib,
    inara,
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
        (_: super:
          with super; {
          })
      ];
    };
  in {
    packages.${system} = {
      default = import ./. {inherit pkgs cloudflareZlib;};
      paper = pkgs.callPackage ./paper/paper.nix {inherit inara;};
    };
    devShells.${system} = {
      default = self.packages.${system}.default.env;
      paper = self.packages.${system}.paper.env;
    };
  };
}
