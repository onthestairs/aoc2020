{
  description = "dias";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };

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
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            my-ghc
            pkgs.haskellPackages.haskell-language-server
          ];
        };
      }
    );
}
