# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{
  description = "QuickCheck Dynamic";


  inputs = {
    iogx.url = "github:input-output-hk/iogx";
  };


  outputs = inputs: inputs.iogx.lib.mkFlake ./. { inherit inputs; };


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
