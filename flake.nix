{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {

    # beam.packages.erlang_27.elixir_1_17
    # packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
    devShells.${system}.default = pkgs.mkShell {
      buildInputs = [
        pkgs.beam.packages.erlang_27.elixir_1_17
        pkgs.beam.packages.erlang_27.elixir-ls
      ];
    };

    };
}
