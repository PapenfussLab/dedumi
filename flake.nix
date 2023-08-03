{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.cloudflareZlib = {
    url = "github:cloudflare/zlib";
    flake = false;
  };
  outputs = {
    self,
    nixpkgs,
    cloudflareZlib,
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
    packages.${system}.default = import ./. {inherit pkgs cloudflareZlib;};
    devShells.${system}.default = self.packages.${system}.default.env;
  };
}
