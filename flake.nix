{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  outputs = {
    self,
    nixpkgs,
  }: let
    system = "x86_64-linux";
    pkgs = import nixpkgs {inherit system;};
  in {
    packages.${system}.default = import ./. {inherit pkgs;};
    devShells.${system}.default = self.packages.${system}.default.env;
  };
}
