{
  description = "QuickCheck Dynamic";


  inputs = {

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    import ./nix/outputs.nix { inherit inputs system; }
  );

  nixConfig = {
    extra-substituters = [
      "https://iohk.cachix.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    accept-flake-config = true;
    allow-import-from-derivation = true;
  };
}
