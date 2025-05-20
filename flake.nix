{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    unstable.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      unstable,
      flake-parts,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      debug = true;
      perSystem =
        {
          config,
          self',
          inputs',
          pkgs,
          system,
          ...
        }:
        let
          unstable = inputs.unstable.legacyPackages.${system};
          hPkgs = unstable.haskell.packages."ghc984";
          devTools = [
            hPkgs.ghc
            hPkgs.ghcid
            hPkgs.fourmolu
            hPkgs.haskell-language-server
            pkgs.zlib
          ];
        in
        {
          # Per-system attributes can be defined here. The self' and inputs'
          # module parameters provide easy access to attributes of the same
          # system.
          #devshells.default = {

          #};
          devShells.default = pkgs.mkShell {
            nativeBuildInputs = devTools;
            LD_LIBRARY_PATH = unstable.lib.makeLibraryPath devTools;
          };

          apps = { };
        };
    };

}
