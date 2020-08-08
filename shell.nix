# shell.nix
{ pkgs ? import <nixpkgs> {} }:

let
  hsPkgs = import ./default.nix { inherit pkgs; };
in
  hsPkgs.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      ipfs
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = true;

    # You might want some extra tools in the shell (optional).

    # Some common tools can be added with the `tools` argument
    tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; stack = "2.1.3.1"};
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = with pkgs.haskellPackages;
      [
        ghcid stm
        aeson
        # base
        bytestring
        envy
        flow
        Glob
        ip
        lens
        monad-logger
        regex-compat
        rio
        servant-client
        servant-server
        stm
        swagger2
        text
        vector
      ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
