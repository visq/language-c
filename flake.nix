{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs: let
    packages = final: p: {
      "language-c" =
        final.haskell.lib.overrideCabal
        (p.callCabal2nixWithOptions "language-c" "${self}" "" {}) {
          doCheck = false;
        };
    };
    overlays = final: prev: {
      haskellPackages = prev.haskellPackages.extend (p: _: packages final p);
    };
  in
    {
      overlays.default = overlays;
    }
    // flake-utils.lib.eachDefaultSystem
    (system: let
      hpkgs =
        (import nixpkgs {
          inherit system;
          overlays = [overlays];
        })
        .haskellPackages;
    in rec {
      packages = {
        default = hpkgs.language-c;
        language-c = hpkgs.language-c;
      };
      devShells = let
        buildInputs = with hpkgs; [
          cabal-install
          ghcid
          haskell-language-server
          hpack
          fourmolu
        ];
        withHoogle = true;
      in {
        default =
          hpkgs.shellFor
          {
            name = "language-c-shell";
            packages = p: [p.language-c];
            inherit buildInputs withHoogle;
          };
      };
    });
}
