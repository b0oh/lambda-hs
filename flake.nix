{
  description = "A Haskell development environment";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc912;

        projectDependencies = with haskellPackages; [
          base
          haskell-src-meta
        ];

        devTools = with haskellPackages; [
          cabal-gild
	        cabal-install
          fourmolu
          ghcid
          haskell-debug-adapter
          haskell-language-server
          hlint
          hoogle
        ];

        nativeDependencies = with pkgs; [
          # e.g., zlib, openssl, etc.
        ];

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = projectDependencies ++ devTools ++ nativeDependencies;

          # Set environment variables
          shellHook = ''
            echo "Entering Haskell development environment"
	    export PS1="[nix-dev] $ "
          '';
        };

        packages.default = haskellPackages.callCabal2nix "shell" ./. { };
      });
}
