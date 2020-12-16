let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  # myReanimate =  pkgs.haskellPackages.reanimate.overrideAttrs (
  #   old: rec {
  #       buildInputs = old.buildInputs ++ [
  #         pkgs.darwin.apple_sdk.frameworks.CoreServices
  #         pkgs.darwin.apple_sdk.frameworks.CoreFoundation
  #         pkgs.darwin.apple_sdk.frameworks.Security
  #         pkgs.darwin.apple_sdk.frameworks.CoreGraphics
  #         pkgs.darwin.apple_sdk.frameworks.Carbon
  #       ];
  #   }
  # );

  haskellDeps = ps: with ps; [
    hlint
    brittany
    ghcide
    base
    relude
    megaparsec
    conduit
    lens
    multiset
    linear
    arithmoi
    monad-loops
  ];
  my-ghc = pkgs.haskellPackages.ghcWithPackages haskellDeps;
in
pkgs.mkShell {
  buildInputs = [
    my-ghc
  ];
}
