{
  description = "QuickCheck Dynamic";


  inputs = {

    iogx = {
      url = "github:input-output-hk/iogx?ref=custom-precommit-hooks";
      inputs.hackage.follows = "hackage";
      inputs.CHaP.follows = "CHaP";
      inputs.haskell-nix.follows = "haskell-nix";
      inputs.nixpkgs.follows = "nixpkgs";
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

    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };
  };


  outputs = inputs: inputs.iogx.lib.mkFlake { 
    inherit inputs;
    repoRoot = ./.;
    systems = [ "x86_64-darwin" "x86_64-linux" "aarch64-darwin" ];
    outputs = import ./nix/outputs.nix;
  };


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
