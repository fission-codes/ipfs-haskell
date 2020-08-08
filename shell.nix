let
  sources  = import ./nix/sources.nix;
  pkgs     = import sources.nixpkgs  {};
  unstable = import sources.unstable {};
in

pkgs.mkShell {
  buildInputs = [
    pkgs.gnumake

    # Haskell
    unstable.ghcid
    unstable.stack
    unstable.stylish-haskell
  ];
}
