{ rosetta ? false }:
  let
    sources  = import ./nix/sources.nix;
    # commands = import ./nix/commands.nix;

    overrides = if rosetta then { system = "x86_64-darwin"; } else {};

    nixos    = import sources.nixos    overrides;
    darwin   = import sources.darwin   overrides;
    unstable = import sources.unstable overrides;

    pkgs  = if darwin.stdenv.isDarwin then darwin else nixos;
   #  tasks = commands {
   #    inherit pkgs;
   #    inherit unstable;
   #  };

    ghc = unstable.ghc;

    deps = {
      common = [
      #  unstable.niv
      ];

      data = [
 #       pkgs.ipfs
   #     pkgs.zlib.dev
      ];

      haskell = [
       # unstable.stack
        # unstable.stylish-haskell
      ];
    };
  in

  unstable.haskell.lib.buildStackProject {
    inherit ghc;
    name = "Fisson";
    nativeBuildInputs = builtins.concatLists [
   #   deps.common
   #   deps.data
   #   deps.haskell
   #   tasks
    ];

    shellHook = ''
      export LANG=C.UTF8
    '';
  }
