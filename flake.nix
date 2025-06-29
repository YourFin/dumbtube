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
            hPkgs.hoogle
            hPkgs.retrie
            hPkgs.implicit-hie
            hPkgs.fourmolu
            hPkgs.haskell-language-server
            stack-wrapped
          ];
          # Wrap Stack to work with our Nix integration. We don't want to modify
          # stack.yaml so non-Nix users don't notice anything.
          # - no-nix: We don't want Stack's way of integrating Nix.
          # --system-ghc    # Use the existing GHC on PATH (will come from this Nix file)
          # --no-install-ghc  # Don't try to install GHC if no matching GHC found on PATH
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack"; # will be available as the usual `stack` in terminal
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --no-nix \
                  --system-ghc \
                  --no-install-ghc \
                "
            '';
          };
          libs = [
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
            buildInputs = libs;
            LD_LIBRARY_PATH = unstable.lib.makeLibraryPath libs;
          };

          packages.default = pkgs.hello;

          apps = { };
        };
    };

}
