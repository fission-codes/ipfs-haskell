{ rosetta ? false }:
  let
    sources  = import ./nix/sources.nix;

    overrides = if rosetta then { system = "x86_64-darwin"; } else {};

    nixos    = import sources.nixos    overrides;
    darwin   = import sources.darwin   overrides;
    unstable = import sources.unstable overrides;

    pkgs  = if darwin.stdenv.isDarwin then darwin else nixos;

    ghc = unstable.ghc;

    deps = {
      haskell = [
       unstable.stack
       unstable.stylish-haskell
      ];
    };
  in

  unstable.haskell.lib.buildStackProject {
    inherit ghc;
    name = "Fisson";
    nativeBuildInputs = builtins.concatLists [
      deps.haskell
    ];

    shellHook = ''
      export LANG=C.UTF8
    '';
  }
