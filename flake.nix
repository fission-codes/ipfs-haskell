{
  description = "IPFS APIs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let

          pkgs = nixpkgs.legacyPackages.${system};

          # Inspired by https://www.tweag.io/blog/2022-06-02-haskell-stack-nix-shell/
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "\
                  --nix \
                  --nix-shell-file=nix/stack-integration.nix \
                "
            '';
          };

          # Wrapper commands for convenience
          commands = import ./nix/commands.nix;
          tasks = commands {
            inherit pkgs;
            inherit stack-wrapped;
          };

          # The default version of HLS (with binary cache) is built with GHC 9.0.1
          # We can get this version working with our current set up, but it builds
          # from source (and takes a long time).
          #
          # The prebuilt package is marked as broken on aarch64-darwin
          haskellPackages = pkgs.haskell.packages.ghc8107;
        in
        {
          devShells.default = pkgs.mkShell {
            name = "fission";
            buildInputs = [
              stack-wrapped
              haskellPackages.haskell-language-server
              pkgs.cachix
              pkgs.zlib
              pkgs.nixpkgs-fmt
              pkgs.stylish-haskell
              tasks
            ];
            NIX_PATH = "nixpkgs=" + pkgs.path;
          };
        }
      );
}
